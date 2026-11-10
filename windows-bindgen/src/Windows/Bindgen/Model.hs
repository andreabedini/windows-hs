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
  , Selection (..)
  , select
  , stringConstantType
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
    -- field offsets. Projected as opaque bytes for now.
    KUnion
  | -- | A struct marked @NativeTypedef@: a C typedef such as @HWND@ or
    -- @BOOL@ wrapping a single field.
    KNativeTypedef
  | KDelegate
  | KInterface
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
    | otherwise -> byName (typeRefNamespace r <> "." <> typeRefName r) (x64Candidates r)
  where
    -- Nested types carry no SupportedArchitecture attribute, but their
    -- enclosing type may have one definition per architecture (CONTEXT),
    -- each with its own nested types. Choose the enclosing type first.
    x64Candidates r = case typeRefScope r of
      Just (ScopeTypeRef enclosing) ->
        [n | e <- x64Candidates enclosing, n <- typeDefNestedTypes e, typeDefName n == typeRefName r]
      _ -> filter supportsX64 (resolveTypeRef r)
    siblings d = findTypeDefs (rowDatabase d) (typeDefNamespace d) (typeDefName d)
    byName name candidates = case filter supportsX64 candidates of
      [d] -> Right (TargetDef d)
      [] -> Left ("no x64 definition of " ++ T.unpack name)
      ds -> Left (show (length ds) ++ " x64 definitions of " ++ T.unpack name)

-- | Whether a definition applies to x64. Definitions without a
-- @SupportedArchitecture@ attribute apply to every architecture.
supportsX64 :: (HasAttributes t) => Row t -> Bool
supportsX64 r = case findAttribute "SupportedArchitectureAttribute" r of
  Just Attribute {attrArgs = [AttrEnum _ (VInt32 arch)]} -> testBit arch 1
  _ -> True

-- | The non-static fields of a struct, union or enum.
instanceFields :: TypeDef -> [Field]
instanceFields t = [f | f <- typeDefFields t, not (hasFlag fieldStatic (fieldFlags f))]

-- | The @Invoke@ method of a delegate, which carries its signature.
delegateInvoke :: TypeDef -> Maybe MethodDef
delegateInvoke t = case [m | m <- typeDefMethods t, methodName m == "Invoke"] of
  [m] -> Just m
  _ -> Nothing

-- | Everything one generated module contains.
data Selection = Selection
  { selFunctions :: [MethodDef]
  , selConstants :: [Field]
  , selTypes :: [TypeDef]
  -- ^ The requested types and every type they depend on.
  }

-- | Expand requested names into a selection.
--
-- A name is either a namespace (selecting everything in it) or
-- @Namespace.Name@, where @Name@ is a type, or a function or constant of the
-- namespace's @Apis@ class. Names that match nothing are returned as errors.
select :: Database -> [Text] -> Either [String] Selection
select db names = case [T.unpack n | (n, []) <- matches] of
  [] -> Right (closeOver (withStringTypes (concatMap snd matches)))
  unknown -> Left ["no type, function or constant named " ++ n | n <- unknown]
  where
    matches = [(n, lookupName n) | n <- names]

    namespaces = Map.fromListWith (++) [(typeDefNamespace t, [t]) | t <- typeDefs db]

    lookupName :: Text -> [Root]
    lookupName n = case Map.lookup n namespaces of
      Just ts -> concatMap everything ts
      Nothing ->
        let (ns, name) = splitName n
            types = [RootType t | t <- findTypeDefs db ns name, classify t /= KApis, supportsX64 t]
            apis = findTypeDefs db ns "Apis"
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
           , t <- findTypeDefs db "Windows.Win32.Foundation" n
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
  KStruct -> concatMap (typeDeps . fieldType) (instanceFields t)
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
