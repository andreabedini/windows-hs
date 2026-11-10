-- | What the generator knows about metadata: the kind of each type
-- definition, which definitions apply to x64, and how a set of requested
-- names expands into everything the generated module must define.
module Windows.Bindgen.Model
  ( Kind (..)
  , classify
  , Target (..)
  , resolveTarget
  , supportsX64
  , instanceFields
  , delegateInvoke
  , interfaceBase
  , interfaceAncestors
  , Selection (..)
  , select
  , stringConstantType
  , freeWith
  , coreType
  ) where

import Data.Bits (testBit, (.&.))
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import WinMD

-- | How a type definition is projected.
data Kind
  = KEnum
  | -- | A struct with sequential layout.
    KStruct
  | -- | A struct with explicit layout: a union, or a struct with explicit
    -- field offsets. Projected as a newtype over its bytes, with a
    -- 'Windows.Core.Member' instance per member.
    KUnion
  | -- | A struct marked @NativeTypedef@: a C typedef such as @HWND@ or
    -- @BOOL@ wrapping a single field.
    KNativeTypedef
  | KDelegate
  | KInterface
  | -- | A COM class: a struct without fields that carries the class
    -- identifier (CLSID) in a @Guid@ attribute, such as @FileOpenDialog@.
    -- Projected as a GUID constant, as in windows-rs.
    KClass
  | -- | The @Apis@ class that holds a namespace's functions and constants.
    KApis
  | -- | A custom attribute class, such as @NativeTypedefAttribute@. These
    -- describe the metadata itself and are not projected.
    KAttribute
  | KOther
  deriving stock (Eq, Show)

classify :: TypeDef -> Kind
classify t
  | hasFlag typeInterface (typeDefFlags t) = KInterface
  | otherwise = case typeDefExtends t >>= typeDefOrRefName of
      Just ("System", "Enum") -> KEnum
      Just ("System", "MulticastDelegate") -> KDelegate
      Just ("System", "Attribute") -> KAttribute
      Just ("System", "ValueType")
        | hasAttribute "NativeTypedefAttribute" t -> KNativeTypedef
        | typeDefFlags t .&. typeLayoutMask == typeExplicitLayout -> KUnion
        | null (instanceFields t), hasAttribute "GuidAttribute" t -> KClass
        | otherwise -> KStruct
      _
        | typeDefName t == "Apis" -> KApis
        | otherwise -> KOther

-- | What a type reference in a signature resolves to.
data Target
  = TargetDef TypeDef
  | -- | @System.Guid@, provided by windows-core.
    TargetGuid
  deriving stock (Eq, Ord, Show)

-- | Resolve a type reference to the definition that applies on x64.
resolveTarget :: TypeDefOrRef -> Either String Target
resolveTarget = \case
  IsTypeSpec s -> Left ("unsupported type spec " ++ show s)
  IsTypeDef d
    | supportsX64 d -> Right (TargetDef d)
    | otherwise -> byName (typeDefFullName d) (siblings d)
  IsTypeRef r
    | (typeRefNamespace r, typeRefName r) == ("System", "Guid") -> Right TargetGuid
    | otherwise -> byName (typeRefNamespace r <> "." <> typeRefName r) (candidates r)
  where
    -- The definitions a reference may mean, for byName to choose the x64
    -- one from. Nested types carry no SupportedArchitecture attribute, but
    -- their enclosing type may have one definition per architecture
    -- (CONTEXT), each with its own nested types: choose the enclosing type
    -- first.
    candidates r = case typeRefScope r of
      Just (ScopeTypeRef enclosing) ->
        [ n
        | e <- filter supportsX64 (candidates enclosing)
        , n <- typeDefNestedTypes e
        , typeDefName n == typeRefName r
        ]
      _ -> resolveTypeRef r
    siblings d = findTypeDefs (rowDatabase d) (typeDefNamespace d) (typeDefName d)
    byName name defs = case filter supportsX64 defs of
      [d] -> Right (TargetDef d)
      []
        | not (null defs) -> Left ("no x64 definition of " ++ T.unpack name)
        -- A few Win32 APIs take WinRT types, which win32metadata refers to
        -- but does not define.
        | "Windows." `T.isPrefixOf` name && not ("Windows.Win32." `T.isPrefixOf` name) ->
            Left (T.unpack name ++ " is a WinRT type, and WinRT is out of scope")
        | otherwise -> Left (T.unpack name ++ " is not defined in the metadata files read")
      ds -> Left (show (length ds) ++ " x64 definitions of " ++ T.unpack name)

-- | Whether a definition applies to x64. Definitions without a
-- @SupportedArchitecture@ attribute apply to every architecture.
supportsX64 :: (HasAttributes t) => Row t -> Bool
supportsX64 r = case findAttribute "SupportedArchitectureAttribute" r of
  Just Attribute {attrArgs = [AttrEnum _ (VInt32 arch)]} -> testBit arch 1
  _ -> True

-- | The name of the type windows-core provides in place of a definition,
-- as windows-rs maps @HRESULT@ to @windows_core::HRESULT@. Generated
-- modules re-export it instead of declaring their own, so that error codes
-- from every module and from the runtime's own error type agree, and the
-- runtime's COM functions ('Windows.Core.cast', 'Windows.Core.release')
-- accept the interfaces of every module.
coreType :: TypeDef -> Maybe Text
coreType t = case (typeDefNamespace t, typeDefName t) of
  ("Windows.Win32.Foundation", "HRESULT") -> Just "HRESULT"
  ("Windows.Win32.System.Com", "IUnknown") -> Just "IUnknown"
  _ -> Nothing

-- | The non-static fields of a struct, union or enum.
instanceFields :: TypeDef -> [Field]
instanceFields t = [f | f <- typeDefFields t, not (hasFlag fieldStatic (fieldFlags f))]

-- | The @Invoke@ method of a delegate, which carries its signature.
delegateInvoke :: TypeDef -> Maybe MethodDef
delegateInvoke t = case [m | m <- typeDefMethods t, methodName m == "Invoke"] of
  [m] -> Just m
  _ -> Nothing

-- | The interface an interface derives from directly. win32metadata gives
-- each at most one (II.22.23: one InterfaceImpl row per interface it
-- implements).
interfaceBase :: TypeDef -> Either String (Maybe TypeDef)
interfaceBase t = case typeDefInterfaces t of
  [] -> Right Nothing
  [r] ->
    resolveTarget r >>= \case
      TargetDef d -> Right (Just d)
      TargetGuid -> Left "an interface derives from System.Guid"
  _ -> Left "an interface derives from several interfaces"

-- | The interfaces an interface derives from, nearest first.
interfaceAncestors :: TypeDef -> Either String [TypeDef]
interfaceAncestors t =
  interfaceBase t >>= \case
    Nothing -> Right []
    Just b -> (b :) <$> interfaceAncestors b

-- | Everything one generated module contains.
data Selection = Selection
  { selFunctions :: [MethodDef]
  , selConstants :: [Field]
  , selTypes :: [TypeDef]
  -- ^ The requested types and every type they depend on.
  }

-- | Expand requested names into a selection from linked metadata files
-- ('linkDatabases').
--
-- A name is either a namespace (selecting everything in it) or
-- @Namespace.Name@, where @Name@ is a type, or a function or constant of the
-- namespace's @Apis@ class. A namespace may span several files. Names that
-- match nothing are returned as errors.
select :: [Database] -> [Text] -> Either [String] Selection
select dbs names = case [T.unpack n | (n, []) <- matches] of
  [] -> Right (closeWithFreeFunctions (withStringTypes (concatMap snd matches)))
  unknown -> Left ["no type, function or constant named " ++ n | n <- unknown]
  where
    matches = [(n, lookupName n) | n <- names]

    namespaces = Map.fromListWith (++) [(typeDefNamespace t, [t]) | t <- allTypeDefs]
    allTypeDefs = concatMap typeDefs dbs
    find ns name = concat [findTypeDefs db ns name | db <- dbs]

    lookupName :: Text -> [Root]
    lookupName n = case Map.lookup n namespaces of
      Just ts -> concatMap everything ts
      Nothing ->
        let (ns, name) = splitName n
            types = [RootType t | t <- find ns name, classify t /= KApis, supportsX64 t]
            apis = find ns "Apis"
            fns = [RootFunction m | a <- apis, m <- typeDefMethods a, methodName m == name, supportsX64 m]
            consts = [RootConstant f | a <- apis, f <- typeDefFields a, fieldName f == name]
         in types ++ fns ++ consts

    -- String constants are typed as PWSTR or PSTR, which their signature
    -- does not mention.
    withStringTypes roots =
      roots
        ++ [ RootType t
           | RootConstant f <- roots
           , Just n <- [stringConstantType f]
           , t <- find "Windows.Win32.Foundation" n
           ]

    -- A function or interface method that names the function to free one
    -- of its outputs (FreeWith) brings that function along, so that its
    -- wrapper can free what it returns, and so can its callers. Interfaces
    -- join the selection through the closure, so close over the roots, add
    -- the functions the selected functions and methods name, and repeat
    -- until there are no new ones.
    closeWithFreeFunctions roots =
      let sel = closeOver roots
          methods =
            selFunctions sel
              ++ [m | t <- selTypes sel, classify t == KInterface, m <- typeDefMethods t]
          new =
            nub
              [ f
              | m <- methods
              , p <- methodParams m
              , Just n <- [freeWith p]
              , f <- Map.findWithDefault [] n functionsByName
              , f `notElem` selFunctions sel
              ]
       in if null new then sel else closeWithFreeFunctions (roots ++ map RootFunction new)

    functionsByName =
      Map.fromListWith
        (++)
        [ (methodName m, [m])
        | t <- allTypeDefs
        , classify t == KApis
        , m <- typeDefMethods t
        , supportsX64 m
        ]

    everything t = case classify t of
      KApis ->
        [RootFunction m | m <- typeDefMethods t, supportsX64 m]
          ++ [RootConstant f | f <- typeDefFields t]
      KAttribute -> []
      _ | supportsX64 t -> [RootType t]
      _ -> []

-- | The type a string constant is projected as: @PSTR@ for constants marked
-- @NativeEncoding("ansi")@, @PWSTR@ for the others, as windows-rs does
-- with @PCSTR@ and @PCWSTR@. 'Nothing' for constants that are not strings.
stringConstantType :: Field -> Maybe Text
stringConstantType f = case constantValue <$> fieldConstant f of
  Just (VString _)
    | Just Attribute {attrArgs = [AttrString (Just "ansi")]} <- findAttribute "NativeEncodingAttribute" f -> Just "PSTR"
    | otherwise -> Just "PWSTR"
  _ -> Nothing

-- | The function that frees what Windows returns in this parameter, from
-- its @FreeWith@ attribute: a bare name such as @CoTaskMemFree@.
freeWith :: Param -> Maybe Text
freeWith p = case findAttribute "FreeWithAttribute" p of
  Just Attribute {attrArgs = [AttrString (Just n)]} -> Just n
  _ -> Nothing

data Root
  = RootType TypeDef
  | RootFunction MethodDef
  | RootConstant Field

-- | Add every type the roots depend on. References that cannot be resolved
-- are left for the emitter to report against the item that uses them.
closeOver :: [Root] -> Selection
closeOver roots =
  Selection
    { selFunctions = nub [m | RootFunction m <- roots]
    , selConstants = nub [f | RootConstant f <- roots]
    , selTypes = sortOn typeDefFullName (Set.toList (go Set.empty initial))
    }
  where
    initial =
      [t | RootType t <- roots]
        ++ concat [sigDeps (methodSignature m) | RootFunction m <- roots]
        ++ concat [typeDeps (fieldType f) | RootConstant f <- roots]
    go seen [] = seen
    go seen (t : ts)
      | t `Set.member` seen = go seen ts
      | otherwise = go (Set.insert t seen) (defDeps t ++ ts)

-- | The definitions a type definition needs in order to be emitted.
defDeps :: TypeDef -> [TypeDef]
defDeps t = case classify t of
  _ | Just _ <- coreType t -> []
  KInterface ->
    either (const []) (maybe [] pure) (interfaceBase t)
      ++ concatMap (sigDeps . methodSignature) (typeDefMethods t)
  KStruct -> concatMap (typeDeps . fieldType) (instanceFields t)
  KUnion -> concatMap (typeDeps . fieldType) (instanceFields t)
  KNativeTypedef -> concatMap (typeDeps . fieldType) (instanceFields t)
  KDelegate -> maybe [] (sigDeps . methodSignature) (delegateInvoke t)
  _ -> []

sigDeps :: MethodSig -> [TypeDef]
sigDeps s = concatMap typeDeps (sigReturnType s : sigParams s)

-- | The definitions referenced anywhere in a type.
typeDeps :: Type -> [TypeDef]
typeDeps = \case
  TPtr t -> typeDeps t
  TByRef t -> typeDeps t
  TPinned t -> typeDeps t
  TSZArray t -> typeDeps t
  TArray t _ -> typeDeps t
  TModified _ _ t -> typeDeps t
  TValueType r -> target r
  TClass r -> target r
  TFnPtr s -> sigDeps s
  _ -> []
  where
    target r = case resolveTarget r of
      Right (TargetDef d) -> [d]
      _ -> []

splitName :: Text -> (Text, Text)
splitName n = case T.breakOnEnd "." n of
  ("", name) -> ("", name)
  (ns, name) -> (T.dropEnd 1 ns, name)
