-- | Emit a Haskell module for a selection.
--
-- Every selected item becomes:
--
-- * an enum: a newtype over its underlying integer, with a pattern synonym
--   per value;
-- * a native typedef (@HWND@, @BOOL@): a newtype over its single field;
-- * a struct: a record with a 'Foreign.Storable.Storable' instance using
--   offsets computed by "Windows.Bindgen.Layout";
-- * a union or explicit-layout struct: a newtype over its bytes;
-- * a delegate: a 'Foreign.Ptr.FunPtr' type and a @mk@ wrapper function;
-- * an interface: a newtype over its pointer, with instances of
--   'Windows.Core.Interface' and of 'Windows.Core.Inherits' for itself and
--   every interface it derives from, and a function per method that calls it
--   through the vtable;
-- * a COM class: a pattern synonym for its class identifier (a 'GUID');
-- * a constant: a pattern synonym (numbers, GUIDs, struct initializers,
--   pointers given as integers, and strings);
-- * a function: a binding that resolves the function on first use with
--   'Windows.Core.resolveProc' and calls it through a @dynamic@ import.
--
-- An item that cannot be emitted is left out, together with everything
-- that refers to it, and reported as a warning.
module Windows.Bindgen.Emit
  ( Config (..)
  , Output (..)
  , generate
  ) where

import Control.Monad (unless, when)
import Data.Either (partitionEithers)
import Data.List (find, findIndices, sortOn)
import Data.Maybe (isJust, maybeToList)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Numeric (showHex)
import WinMD
import Windows.Bindgen.Layout
import Windows.Bindgen.Model
import Windows.Bindgen.Names

data Config = Config
  { cfgModuleName :: Text
  , cfgHeader :: [Text]
  -- ^ Lines for the module's leading comment, such as how it was generated.
  , cfgSys :: Bool
  -- ^ Emit every function with its C signature, as windows-sys does,
  -- instead of checking errors and returning out parameters as windows-rs
  -- does.
  }

data Output = Output
  { outModule :: Text
  , outWarnings :: [String]
  , outStructs :: [(TypeDef, Text)]
  -- ^ Emitted structs, unions and typedefs with their Haskell names, for
  -- layout checks.
  }

-- | A piece of the generated module.
data Decl = Decl
  { declExports :: [Text]
  , declBody :: Text
  }

data Ctx = Ctx
  { ctxTypes :: Map TypeDef Text
  -- ^ Haskell names of the types that will be emitted.
  }

generate :: Config -> Selection -> Output
generate cfg sel =
  Output
    { outModule = render cfg (typeDecls ++ constDecls ++ fnDecls)
    , outWarnings = typeWarnings ++ constWarnings ++ fnWarnings
    , outStructs =
        [ (t, n)
        | (t, n) <- Map.toList (ctxTypes ctx)
        , classify t `elem` [KStruct, KUnion, KNativeTypedef]
        ]
    }
  where
    allNames = typeNames (selTypes sel)
    (ctx, typeWarnings) = settle (Ctx allNames) []

    -- Drop types that fail to emit until every remaining type emits.
    settle c acc =
      let failures = [(t, e) | t <- Map.keys (ctxTypes c), Left e <- [emitType c Map.empty t]]
       in if null failures
            then (c, reverse acc)
            else
              settle
                (Ctx (foldr (Map.delete . fst) (ctxTypes c) failures))
                (reverse [warn (typeDefFullName t) e | (t, e) <- failures] ++ acc)

    -- Pattern synonyms share the constructor namespace with type names.
    enumMembers =
      [ (t, f)
      | t <- Map.keys (ctxTypes ctx)
      , classify t == KEnum
      , f <- typeDefFields t
      , hasFlag fieldLiteral (fieldFlags f)
      ]
    constantList = sortOn fieldName (selConstants sel)
    patternNames =
      Map.fromList $
        zip
          (map (Right . snd) enumMembers ++ map Left constantList)
          ( dedupe
              (Set.fromList (Map.elems (ctxTypes ctx)) <> reservedTypes)
              (map (conName . fieldName . snd) enumMembers ++ map (conName . fieldName) constantList)
          )
    memberNames = Map.fromList [(f, n) | (Right f, n) <- Map.toList patternNames]

    typeDecls =
      [ d
      | t <- sortOn (ctxTypes ctx Map.!) (Map.keys (ctxTypes ctx))
      , Right d <- [emitType ctx memberNames t]
      ]

    (constWarnings, constDecls) =
      partitionEithers
        [ either (Left . warn (fieldName f)) Right (emitConstant ctx (patternNames Map.! Left f) f)
        | f <- constantList
        ]

    functionList = sortOn methodName (selFunctions sel)
    delegateMks = [mkName n | (t, n) <- Map.toList (ctxTypes ctx), classify t == KDelegate]
    -- Methods of the emitted interfaces, in the order of their interfaces'
    -- names.
    methodList =
      [ (t, iface, raw, k, m)
      | (t, iface) <- sortOn snd (Map.toList (ctxTypes ctx))
      , classify t == KInterface
      , Nothing <- [coreType t]
      , (raw, k, m) <- interfaceMethods iface t
      ]
    callableNames =
      dedupe
        (Set.fromList delegateMks)
        (map (valueName . methodName) functionList ++ [raw | (_, _, raw, _, _) <- methodList])
    (functionNames, methodNames) = splitAt (length functionList) callableNames
    callables =
      [ (methodName m, n, Export <$> maybe (Left "no DllImport") Right (methodImplMap m), m)
      | (m, n) <- zip functionList functionNames
      ]
        ++ [ (typeDefFullName t <> "." <> methodName m, n, Right (Method iface k), m)
           | ((t, iface, _, k, m), n) <- zip methodList methodNames
           ]
    emitCallable frees (_, n, callee, m) = callee >>= \c -> emitFunction (cfgSys cfg) ctx frees n c m
    -- The functions that emit, by C name, for wrappers that free what they
    -- return with one of them (FreeWith).
    emitted =
      Map.fromList
        [ (methodName m, (n, m))
        | c@(_, n, Right (Export _), m) <- callables
        , Right _ <- [emitCallable Map.empty c]
        ]
    (fnWarnings, fnDecls) =
      partitionEithers
        [ either (Left . warn label) Right (emitCallable emitted c)
        | c@(label, _, _, _) <- callables
        ]

    warn name e = T.unpack name ++ ": " ++ e

-- | Type names imported into the generated module.
reservedTypes :: Set.Set Text
reservedTypes =
  Set.fromList
    [ "CBool"
    , "Double"
    , "FixedArray"
    , "Float"
    , "FunPtr"
    , "GUID"
    , "HRESULT"
    , "IO"
    , "IUnknown"
    , "Int16"
    , "Int32"
    , "Int64"
    , "Int8"
    , "IntPtr"
    , "Ptr"
    , "Word16"
    , "Word32"
    , "Word64"
    , "Word8"
    , "WordPtr"
    ]

-- | Haskell names for type definitions. A nested type is named after its
-- enclosing type and its position in it, as windows-rs does
-- (@SYSTEM_INFO_0@).
typeNames :: [TypeDef] -> Map TypeDef Text
typeNames ts =
  Map.fromList $
    [(t, n) | t <- ts, Just n <- [coreType t]]
      ++ zip generated (dedupe reservedTypes (map rawName generated))
  where
    generated = [t | t <- ts, Nothing <- [coreType t]]
    rawName t = case typeDefEnclosingType t of
      Just e ->
        let ix = length (takeWhile (/= t) (typeDefNestedTypes e))
         in rawName e <> "_" <> T.pack (show ix)
      Nothing -> conName (typeDefName t)

mkName :: Text -> Text
mkName n = "mk" <> n

-- | The start of the names of an interface's methods: @iFileDialog_@.
methodPrefix :: Text -> Text
methodPrefix iface = valueName iface <> "_"

-- | The methods of an emitted interface: their names before 'dedupe', and
-- their vtable slots. An overloaded name is numbered from its second
-- occurrence (@SetAttributeValue2@), as in windows-rs.
interfaceMethods :: Text -> TypeDef -> [(Text, Int, MethodDef)]
interfaceMethods iface t =
  [ (methodPrefix iface <> overload k (methodName m), base + k, m)
  | (k, m) <- zip [0 ..] ms
  ]
  where
    ms = typeDefMethods t
    base = either (const 0) (sum . map (length . typeDefMethods)) (interfaceAncestors t)
    overload k n = case length (filter ((== n) . methodName) (take k ms)) of
      0 -> n
      c -> n <> tshow (c + 1)

------------------------------------------------------------------------------
-- Types

typeName :: Ctx -> TypeDef -> Either String Text
typeName ctx t = case Map.lookup t (ctxTypes ctx) of
  Just n -> Right n
  Nothing -> Left ("depends on " ++ T.unpack (typeDefFullName t) ++ ", which is not emitted")

-- | The Haskell type for a metadata type in a struct field or a signature.
hsType :: Ctx -> Type -> Either String Text
hsType ctx = \case
  TVoid -> Right "()"
  TBool -> Right "CBool"
  TChar -> Right "Word16"
  TInt8 -> Right "Int8"
  TUInt8 -> Right "Word8"
  TInt16 -> Right "Int16"
  TUInt16 -> Right "Word16"
  TInt32 -> Right "Int32"
  TUInt32 -> Right "Word32"
  TInt64 -> Right "Int64"
  TUInt64 -> Right "Word64"
  TFloat32 -> Right "Float"
  TFloat64 -> Right "Double"
  TIntPtr -> Right "IntPtr"
  TUIntPtr -> Right "WordPtr"
  TPtr t -> ("Ptr " <>) . atom <$> hsType ctx t
  TModified _ _ t -> hsType ctx t
  TArray t shape -> do
    e <- hsType ctx t
    Right ("FixedArray " <> T.pack (show (product (arraySizes shape))) <> " " <> atom e)
  TFnPtr sig -> ("FunPtr " <>) . atom <$> ffiFunctionType ctx Nothing sig
  TValueType r ->
    resolveTarget r >>= \case
      TargetGuid -> Right "GUID"
      TargetDef d -> typeName ctx d
  TClass r ->
    resolveTarget r >>= \case
      TargetDef d
        | classify d == KInterface -> typeName ctx d
        | classify d == KDelegate -> typeName ctx d
      _ -> Left ("unsupported class type " ++ T.unpack (renderType (TClass r)))
  t -> Left ("unsupported type " ++ T.unpack (renderType t))

-- | Parenthesise a type that is not a single token.
atom :: Text -> Text
atom t
  | T.any (== ' ') t && not (T.isPrefixOf "(" t && T.isSuffixOf ")" t) = "(" <> t <> ")"
  | otherwise = t

emitType :: Ctx -> Map Field Text -> TypeDef -> Either String Decl
emitType ctx members t = do
  name <- typeName ctx t
  let doc = docComment t
  case classify t of
    _ | Just _ <- coreType t -> Right Decl {declExports = [name <> " (..)"], declBody = ""}
    KEnum -> do
      base <- case instanceFields t of
        [f] -> hsType ctx (fieldType f)
        _ -> Left "enum without a single value__ field"
      let flags = hasAttribute "FlagsAttribute" t
          values =
            [ (members Map.! f, v)
            | f <- typeDefFields t
            , hasFlag fieldLiteral (fieldFlags f)
            , Just c <- [fieldConstant f]
            , Just v <- [literal (constantValue c)]
            , Map.member f members
            ]
          classes = "P.Eq, P.Ord, S.Storable" <> (if flags then ", B.Bits" else "")
      Right
        Decl
          { declExports = (name <> " (..)") : ["pattern " <> p | (p, _) <- values]
          , declBody =
              T.unlines $
                doc
                  ++ [ "newtype " <> name <> " = " <> name <> " " <> atom base
                     , "  deriving stock (P.Show)"
                     , "  deriving newtype (" <> classes <> ")"
                     ]
                  ++ concat
                    [ ["", "pattern " <> p <> " :: " <> name, "pattern " <> p <> " = " <> name <> " " <> v]
                    | (p, v) <- values
                    ]
          }
    KNativeTypedef -> do
      inner <- case instanceFields t of
        [f] -> hsType ctx (fieldType f)
        _ -> Left "typedef without a single field"
      Right
        Decl
          { declExports = [name <> " (..)"]
          , declBody =
              T.unlines $
                doc
                  ++ [ "newtype " <> name <> " = " <> name <> " " <> atom inner
                     , "  deriving stock (P.Show)"
                     , "  deriving newtype (P.Eq, P.Ord, S.Storable)"
                     ]
                  -- String parameters also take a PWSTR: string constants,
                  -- resources given by number (IDC_ARROW), raw pointers.
                  ++ [ l
                     | (typeDefNamespace t, typeDefName t) == ("Windows.Win32.Foundation", "PWSTR")
                     , l <-
                        [ ""
                        , "instance Core.WideString " <> name <> " where"
                        , "  withWideString (" <> name <> " p) k = k p"
                        ]
                     ]
          }
    KStruct -> do
      Layout size align <- defLayout t
      fields <- structFields t
      let fieldNames = dedupe Set.empty [valueName (fieldName f) | (f, _, _) <- fields]
      types <- mapM (\(f, _, _) -> hsType ctx (fieldType f)) fields
      let vars = ["f" <> T.pack (show i) | i <- [0 .. length fields - 1]]
          offsets = [off | (_, off, _) <- fields]
          record = case zip fieldNames types of
            [] -> ["data " <> name <> " = " <> name]
            (n0, t0) : rest ->
              ("data " <> name <> " = " <> name)
                : ("  { " <> n0 <> " :: !" <> atom t0)
                : ["  , " <> n <> " :: !" <> atom ty | (n, ty) <- rest]
                ++ ["  }"]
          peekBody = case offsets of
            [] -> "P.pure " <> name
            o : os ->
              name
                <> " P.<$> S.peekByteOff p "
                <> tshow o
                <> T.concat [" P.<*> S.peekByteOff p " <> tshow x | x <- os]
          pokeBody = case zip offsets vars of
            [] -> "P.pure ()"
            ps -> T.intercalate " P.>> " ["S.pokeByteOff p " <> tshow o <> " " <> v | (o, v) <- ps]
      Right
        Decl
          { declExports = [name <> " (..)"]
          , declBody =
              T.unlines $
                doc
                  ++ record
                  ++ [ "  deriving stock (P.Eq, P.Ord, P.Show)"
                     , ""
                     , "instance S.Storable " <> name <> " where"
                     , "  sizeOf _ = " <> tshow size
                     , "  alignment _ = " <> tshow align
                     , "  peek " <> (if null offsets then "_" else "p") <> " = " <> peekBody
                     , "  poke " <> (if null vars then "_" else "p") <> " (" <> T.unwords (name : vars) <> ") = " <> pokeBody
                     ]
          }
    KUnion -> do
      Layout size align <- defLayout t
      fields <- structFields t
      let fieldNames = dedupe Set.empty [valueName (fieldName f) | (f, _, _) <- fields]
          -- A member whose type is not emitted is left out; the bytes are
          -- still there.
          unionMembers =
            [ (n, ty, off)
            | (n, (f, off, _)) <- zip fieldNames fields
            , Right ty <- [hsType ctx (fieldType f)]
            ]
          memberInstances (n, ty, off) =
            [ ""
            , "instance Core.Member " <> tshow n <> " " <> name <> " " <> atom ty <> " where"
            , "  memberOffset = " <> tshow off
            , ""
            , "instance R.HasField " <> tshow n <> " " <> name <> " " <> atom ty <> " where"
            , "  getField = Core.getMember @" <> tshow n
            ]
      Right
        Decl
          { declExports = [name <> " (..)"]
          , declBody =
              T.unlines $
                commentLines
                  ( doc
                      ++ [ "-- | A union or explicit-layout struct of " <> tshow size <> " bytes. Read a member"
                         , "-- as a field (@u.member@); build one with 'Core.setMember' and 'Core.zeroed'."
                         ]
                  )
                  ++ [ "newtype " <> name <> " = " <> name <> " (FixedArray " <> tshow size <> " Word8)"
                     , "  deriving stock (P.Eq, P.Ord, P.Show)"
                     , ""
                     , "instance S.Storable " <> name <> " where"
                     , "  sizeOf _ = " <> tshow size
                     , "  alignment _ = " <> tshow align
                     , "  peek p = " <> name <> " P.<$> S.peek (F.castPtr p)"
                     , "  poke p (" <> name <> " b) = S.poke (F.castPtr p) b"
                     ]
                  ++ concatMap memberInstances unionMembers
          }
    KDelegate -> do
      invoke <- maybe (Left "delegate without an Invoke method") Right (delegateInvoke t)
      fn <- ffiFunctionType ctx Nothing (methodSignature invoke)
      Right
        Decl
          { declExports = [name, mkName name]
          , declBody =
              T.unlines $
                doc
                  ++ [ "type " <> name <> " = FunPtr (" <> fn <> ")"
                     , ""
                     , "-- | Wrap a Haskell function as a '" <> name <> "'. Free it with 'F.freeHaskellFunPtr' when it is no longer needed."
                     , "foreign import ccall \"wrapper\""
                     , "  " <> mkName name <> " :: (" <> fn <> ") -> IO " <> name
                     ]
          }
    KInterface -> do
      ancestors <- interfaceAncestors t >>= mapM (typeName ctx)
      iid <- case findAttribute "GuidAttribute" t of
        Just a -> maybe (Left "malformed Guid attribute") guidLit (mapM attrInteger (attrArgs a))
        Nothing -> guidLit (replicate 11 0)
      let com = "IUnknown" `elem` ancestors
      Right
        Decl
          { declExports = [name <> " (..)"]
          , declBody =
              T.unlines $
                commentLines
                  ( doc
                      ++ if com
                        then
                          [ "-- | A COM interface. Its methods are the functions named @" <> methodPrefix name <> "...@, and those of"
                          , "-- the interfaces it derives from. Release a reference with 'Core.release'."
                          ]
                        else
                          [ "-- | An interface without a reference count (not derived from @IUnknown@). Its methods are"
                          , "-- the functions named @" <> methodPrefix name <> "...@, and those of the interfaces it derives from."
                          ]
                  )
                  ++ [ "newtype " <> name <> " = " <> name <> " (Ptr ())"
                     , "  deriving stock (P.Show)"
                     , "  deriving newtype (P.Eq, P.Ord, S.Storable)"
                     , ""
                     , "instance Core.Interface " <> name <> " where"
                     , "  interfaceId = " <> litExpr iid
                     , "  toRaw (" <> name <> " p) = p"
                     , "  fromRaw = " <> name
                     ]
                  ++ concat [["", "instance Core.Inherits " <> a <> " " <> name] | a <- name : ancestors]
          }
    KClass -> do
      clsid <- case findAttribute "GuidAttribute" t of
        Just a -> maybe (Left "malformed Guid attribute") guidLit (mapM attrInteger (attrArgs a))
        Nothing -> Left "a class without a Guid attribute"
      Right
        Decl
          { declExports = ["pattern " <> name]
          , declBody =
              T.unlines $
                commentLines (doc ++ ["-- | The class identifier (CLSID) of the COM class " <> typeDefName t <> "."])
                  ++ [ "pattern " <> name <> " :: GUID"
                     , "pattern " <> name <> " = " <> litExpr clsid
                     ]
          }
    k -> Left ("cannot emit a " ++ show k)

------------------------------------------------------------------------------
-- Calling convention

-- | How a value crosses the FFI boundary.
data Conv
  = -- | As it is.
    Plain
  | -- | A struct of 1, 2, 4 or 8 bytes, reinterpreted as an integer.
    AsWord
  | -- | Any other struct: by pointer to a copy (parameters), or through a
    -- hidden result pointer (return values).
    ByRef
  deriving stock (Eq)

data Slot = Slot
  { slotHs :: Text
  , slotFfi :: Text
  , slotConv :: Conv
  }

slot :: Ctx -> Type -> Either String Slot
slot ctx ty = do
  let t = case strip ty of
        TArray e _ -> TPtr e
        other -> other
  hs <- hsType ctx t
  byValue <- case t of
    TValueType r ->
      resolveTarget r >>= \case
        TargetGuid -> Right True
        TargetDef d -> Right (classify d `elem` [KStruct, KUnion])
    _ -> Right False
  if not byValue
    then Right (Slot hs hs Plain)
    else do
      Layout size _ <- typeLayout t
      Right $ case size of
        1 -> Slot hs "Word8" AsWord
        2 -> Slot hs "Word16" AsWord
        4 -> Slot hs "Word32" AsWord
        8 -> Slot hs "Word64" AsWord
        _ -> Slot hs ("Ptr " <> atom hs) ByRef

-- | How a result crosses the FFI boundary. A C++ member function, such as a
-- COM method, returns every struct through a hidden pointer, which comes
-- after @this@ (MSVC x64 ABI, \"Return values\"), so small structs are not
-- returned as integers there as they are from C functions.
resultSlot :: Ctx -> Bool -> Type -> Either String Slot
resultSlot ctx method ty = do
  s <- slot ctx ty
  Right $
    if method && slotConv s == AsWord
      then Slot (slotHs s) ("Ptr " <> atom (slotHs s)) ByRef
      else s

-- | The FFI type of a function pointer with this signature, given the type
-- of @this@ for a method. Structs passed by value appear as the integers
-- or pointers the ABI uses.
ffiFunctionType :: Ctx -> Maybe Text -> MethodSig -> Either String Text
ffiFunctionType ctx this sig = do
  checkSig sig
  params <- mapM (slot ctx) (sigParams sig)
  ret <- resultSlot ctx (isJust this) (sigReturnType sig)
  let (extra, result) = case slotConv ret of
        ByRef -> ([slotFfi ret], slotFfi ret)
        _ -> ([], slotFfi ret)
  Right (T.intercalate " -> " (maybeToList this ++ extra ++ map slotFfi params ++ ["IO " <> atom result]))

checkSig :: MethodSig -> Either String ()
checkSig sig = do
  when (sigCallingConvention sig == CallVararg || not (null (sigVarargParams sig))) $
    Left "variadic functions are not supported"
  when (sigGenericParamCount sig /= 0) $ Left "generic methods are not supported"

------------------------------------------------------------------------------
-- Functions

-- | What a binding calls.
data Callee
  = -- | A function exported by a DLL.
    Export ImplMap
  | -- | A method of an interface: the interface's Haskell name, and the
    -- method's slot in its vtable.
    Method Text Int

emitFunction :: Bool -> Ctx -> Map Text (Text, MethodDef) -> Text -> Callee -> MethodDef -> Either String Decl
emitFunction sys ctx frees name callee m = do
  let sig = methodSignature m
      this = case callee of
        Method iface _ -> Just iface
        Export _ -> Nothing
      setsLastError = case callee of
        Export im -> hasFlag pinvokeSupportsLastError (implMapFlags im)
        Method {} -> False
  checkSig sig
  params <- mapM (slot ctx) (sigParams sig)
  ret <- resultSlot ctx (isJust this) (sigReturnType sig)
  ffiType <- ffiFunctionType ctx this sig
  shape <- if sys then Right Raw else functionShape ctx frees m setsLastError sig
  strings <- if sys then Right (map (const Nothing) params) else mapM (wideParam ctx m) (zip [1 ..] (sigParams sig))
  interfaces <- if sys then Right (map (const Nothing) params) else mapM (interfaceParam ctx m) (zip [1 ..] (sigParams sig))
  buffers <- if sys then Right Map.empty else wideBuffers ctx m (sigParams sig)
  let procName = "proc'" <> name
      callName = "call'" <> name
      n = length params
      args = ["a" <> tshow i | i <- [1 .. n]]
      -- A returned out parameter is passed a pointer the wrapper allocates,
      -- and a string buffer one sized by its count parameter: neither is an
      -- argument of the wrapper. Nor are the IID and the interface pointer
      -- of a query.
      outIndex = case shape of
        ReturnOut {} -> Just (n - 1)
        ReturnString {} -> Just (n - 1)
        Query _ o -> Just o
        _ -> Nothing
      guidIndex = case shape of
        Query g _ -> Just g
        _ -> Nothing
      hsIndices = [i | i <- [0 .. n - 1], Just i /= outIndex, Just i /= guidIndex, i `Map.notMember` buffers]
      -- String parameters are type variables, s1, s2, ..., and interfaces
      -- that are not optional i1, i2, ...
      stringVars = Map.fromList (zip [i | (i, Just _) <- zip [0 :: Int ..] strings] ["s" <> tshow k | k <- [1 :: Int ..]])
      interfaceVars = Map.fromList (zip [i | (i, Just (_, False)) <- zip [0 :: Int ..] interfaces] ["i" <> tshow k | k <- [1 :: Int ..]])
      argIn i
        | Just i == outIndex = "o"
        | Just i == guidIndex = "riid"
        | Just (_, con, _) <- Map.lookup i buffers = "(" <> con <> " " <> a <> ")"
        | Just (con, _) <- strings !! i = "(" <> con <> " " <> a <> "')"
        | Just (iface, False) <- interfaces !! i = "(Core.upcast @" <> iface <> " " <> a <> ")"
        | Just (_, True) <- interfaces !! i = "(Core.orNull " <> a <> ")"
        | slotConv (params !! i) == Plain = a
        | otherwise = a <> "'"
        where
          a = args !! i
      (target, thisArg) = case callee of
        Export _ -> (procName, [])
        Method {} -> ("f", ["this"])
      callArgs = thisArg ++ ["r" | slotConv ret == ByRef] ++ map argIn [0 .. n - 1]
      call = T.unwords (callName : target : callArgs)
      result = case slotConv ret of
        Plain -> call
        AsWord -> call <> " P.>>= Core.wordToStruct"
        ByRef -> "Core.returnStruct (\\r -> " <> call <> ")"
      checked = case shape of
        Raw -> result
        CheckHResult -> result <> " P.>>= Core.check"
        CheckBool con -> result <> " P.>>= \\(" <> con <> " r) -> if r P.== 0 then Core.throwLastError else P.pure ()"
        CheckHandle con invalid -> result <> " P.>>= \\h@(" <> con <> " v) -> if " <> invalid "v" <> " then Core.throwLastError else P.pure h"
        ReturnOut hr _ ->
          "M.alloca P.$ \\o -> " <> result <> (if hr then " P.>>= Core.check" else "") <> " P.>> S.peek o"
        -- The pointer starts as NULL, so that a function that fails
        -- without writing it frees nothing.
        ReturnString hr con free ->
          "M.alloca P.$ \\o -> S.poke o (" <> con <> " F.nullPtr) P.>> "
            <> result
            <> (if hr then " P.>>= \\r -> " else " P.>> ")
            <> "S.peek o P.>>= \\("
            <> con
            <> " p) -> Core.takeWideString p ("
            <> free
            <> " (F.castPtr p)) "
            <> (if hr then "(Core.check r)" else "(P.pure ())")
        Query {} -> "Core.query (\\riid o -> " <> result <> ")"
      shapeResult = case shape of
        Raw -> slotHs ret
        CheckHResult -> "()"
        CheckBool _ -> "()"
        CheckHandle {} -> slotHs ret
        ReturnOut _ t -> t
        ReturnString {} -> "Core.OsString"
        Query {} -> "t"
      -- String buffers are read after the call and returned after its
      -- result.
      bufferReads = [(args !! b, args !! c) | (b, (c, _, _)) <- Map.toList buffers]
      keepResult = shapeResult /= "()"
      tuple xs = case xs of
        [x] -> x
        _ -> "(" <> T.intercalate ", " xs <> ")"
      final
        | null bufferReads = checked
        | otherwise =
            "("
              <> checked
              <> ") P.>>= \\"
              <> (if keepResult then "r' -> " else "_ -> ")
              <> T.concat ["Core.peekWideBuffer " <> b <> " " <> c <> " P.>>= \\" <> b <> "' -> " | (b, c) <- bufferReads]
              <> "P.pure "
              <> tuple (["r'" | keepResult] ++ [b <> "'" | (b, _) <- bufferReads])
      hsResult
        | null bufferReads = shapeResult
        | otherwise = tuple ([shapeResult | keepResult] ++ ["Core.OsString" | _ <- bufferReads])
      wrappers =
        [ "Core.vtableEntry this " <> tshow k <> " P.>>= \\f ->"
        | Method _ k <- [callee]
        ]
          ++ [w | i <- hsIndices, Just w <- [wrapper (args !! i) (slotConv (params !! i)) (strings !! i)]]
          ++ ["Core.withWideBuffer " <> c <> " P.$ \\" <> b <> " ->" | (b, c) <- bufferReads]
      wrapper a conv str = case (str, conv) of
        (Just _, _) -> Just ("Core.withWideString " <> a <> " P.$ \\" <> a <> "' ->")
        (_, AsWord) -> Just ("Core.structToWord " <> a <> " P.>>= \\" <> a <> "' ->")
        (_, ByRef) -> Just ("Core.withStructCopy " <> a <> " P.$ \\" <> a <> "' ->")
        (_, Plain) -> Nothing
      argType i
        | Just v <- Map.lookup i stringVars = v
        | Just v <- Map.lookup i interfaceVars = v
        | Just (iface, True) <- interfaces !! i = "P.Maybe " <> iface
        | otherwise = slotHs (params !! i)
      argTypes = ["this" | isJust this] ++ map argType hsIndices
      -- A type application sets the first type variable of the signature,
      -- so t comes before this: iShellItem_BindToHandler @IStream.
      constraints =
        ["Core.Interface t" | Query {} <- [shape]]
          ++ ["Core.Inherits " <> iface <> " this" | Just iface <- [this]]
          ++ ["Core.Inherits " <> iface <> " " <> (interfaceVars Map.! i) | (i, Just (iface, False)) <- zip [0 ..] interfaces]
          ++ ["Core.WideString " <> v | v <- Map.elems stringVars]
      constraint = case constraints of
        [] -> ""
        [c] -> c <> " => "
        cs -> "(" <> T.intercalate ", " cs <> ") => "
      hsSig = constraint <> T.intercalate " -> " (argTypes ++ ["IO " <> atom hsResult])
      thisPat = ["(Core.upcast @" <> iface <> " -> this)" | Just iface <- [this]]
      optionalStrings = [o | Just (_, Just o) <- strings]
      optionalInterfaces = [paramName p | (i, Just (_, True)) <- zip [1 ..] interfaces, Just p <- [paramAt m i]]
      stringComment =
        [ "-- | Optional: " <> T.intercalate ", " ["@" <> o <> "@" | o <- optionalStrings ++ optionalInterfaces] <> " can be @Nothing@ (NULL)."
        | not (null optionalStrings && null optionalInterfaces)
        ]
      bufferComment =
        [ "-- | Returns the string written to @" <> bn <> "@, up to its first NUL. The wrapper allocates that buffer with room for @" <> cn <> "@ UTF-16 units."
        | (_, (_, _, (bn, cn))) <- Map.toList buffers
        ]
          ++ [ "-- | Pass NULL for @" <> o <> "@: a pointer into the buffer would not outlive the wrapper."
             | not (Map.null buffers)
             , (p, ty) <- zip (sortOn paramSequence [q | q <- methodParams m, paramSequence q > 0]) (sigParams sig)
             , hasFlag paramOut (paramFlags p)
             , TPtr t <- [strip ty]
             , isPwstr t
             , let o = paramName p
             ]
      foreignImport =
        [ "foreign import ccall safe \"dynamic\""
        , "  " <> callName <> " :: FunPtr (" <> ffiType <> ") -> " <> ffiType
        ]
  tail' <- case callee of
    Method {} -> Right foreignImport
    Export im -> do
      let dll = moduleRefName (implMapScope im)
          entry = implMapImportName im
      unless (T.all (\c -> c /= '"' && c /= '\\') (dll <> entry)) $ Left "unexpected characters in DLL or entry point name"
      Right $
        [ procName <> " :: FunPtr (" <> ffiType <> ")"
        , procName <> " = U.unsafePerformIO (Core.resolveProc \"" <> dll <> "\" \"" <> entry <> "\")"
        , "{-# NOINLINE " <> procName <> " #-}"
        , ""
        ]
          ++ foreignImport
  Right
    Decl
      { declExports = [name]
      , declBody =
          T.unlines $
            commentLines (docComment m ++ shapeComment shape ++ bufferComment ++ stringComment)
              ++ [ name <> " :: " <> hsSig
                 , T.unwords (name : thisPat ++ map (args !!) hsIndices) <> " ="
                 ]
              ++ map ("  " <>) (wrappers ++ [final])
              ++ [""]
              ++ tail'
      }

-- | The parameter row with this sequence number (1 for the first).
paramAt :: MethodDef -> Int -> Maybe Param
paramAt m i = find ((== i) . fromIntegral . paramSequence) (methodParams m)

-- | An interface the function only reads: an @[In]@ parameter whose type
-- is an interface. The wrapper takes any interface that derives from it
-- ('Windows.Core.Inherits'), or @Maybe@ the interface where it is
-- optional, as windows-rs takes a @Param@. Gives the interface's name and
-- whether it is optional.
interfaceParam :: Ctx -> MethodDef -> (Int, Type) -> Either String (Maybe (Text, Bool))
interfaceParam ctx m (i, ty) = case (strip ty, paramAt m i) of
  (TClass r, Just p)
    | Right (TargetDef d) <- resolveTarget r
    , classify d == KInterface
    , let fl = paramFlags p
    , hasFlag paramIn fl
    , not (hasFlag paramOut fl) -> do
        iface <- typeName ctx d
        Right (Just (iface, hasFlag paramOptional fl))
  _ -> Right Nothing

-- | Buffers the function writes a string to: an @[Out] PWSTR@ parameter
-- whose size in UTF-16 units is an integer parameter (@NativeArrayInfo@
-- with @CountParamIndex@) that only it uses. The wrapper allocates the
-- buffer from that size and returns the string; windows-rs 0.58 passes the
-- pair as one slice in the same cases. Buffers whose size is passed by
-- pointer (@GetComputerNameW@) stay raw, as there.
--
-- Maps the buffer's index to its size parameter's index, the constructor
-- of @PWSTR@, and the names of both parameters.
wideBuffers :: Ctx -> MethodDef -> [Type] -> Either String (Map Int (Int, Text, (Text, Text)))
wideBuffers ctx m tys = Map.fromList <$> sequence candidates
  where
    rows = Map.fromList [(fromIntegral (paramSequence p) - 1, p) | p <- methodParams m, paramSequence p > 0]
    index attr key p = case findAttribute attr p of
      Just a | Just (AttrValue v) <- lookup key (attrNamedArgs a) -> valueInt v
      _ -> Nothing
    -- Every parameter that gives the size of another.
    sizeRefs =
      [c | p <- Map.elems rows, Just c <- [index "NativeArrayInfoAttribute" "CountParamIndex" p]]
        ++ [c | p <- Map.elems rows, Just c <- [index "MemorySizeAttribute" "BytesParamIndex" p]]
    candidates =
      [ (\con -> (i, (c, con, (paramName p, paramName q)))) <$> typeName ctx d
      | (i, p) <- Map.toList rows
      , i < length tys
      , Just d <- [pwstr (tys !! i)]
      , let fl = paramFlags p
      , hasFlag paramOut fl
      , not (hasFlag paramIn fl)
      , Just c <- [index "NativeArrayInfoAttribute" "CountParamIndex" p]
      , c /= i
      , c >= 0
      , c < length tys
      , length (filter (== c) sizeRefs) == 1
      , Just q <- [Map.lookup c rows]
      , hasFlag paramIn (paramFlags q)
      , not (hasFlag paramOut (paramFlags q))
      , isInteger (strip (tys !! c))
      ]
    pwstr t = case strip t of
      TValueType r
        | Right (TargetDef d) <- resolveTarget r
        , isPwstr t ->
            Just d
      _ -> Nothing
    isInteger = (`elem` [TInt8, TUInt8, TInt16, TUInt16, TInt32, TUInt32, TInt64, TUInt64, TIntPtr, TUIntPtr])
    valueInt = \case
      VInt16 x -> Just (fromIntegral x)
      VInt32 x -> Just (fromIntegral x)
      VUInt16 x -> Just (fromIntegral x)
      _ -> Nothing

-- | @PWSTR@.
isPwstr :: Type -> Bool
isPwstr t = case strip t of
  TValueType r
    | Right (TargetDef d) <- resolveTarget r ->
        (typeDefNamespace d, typeDefName d) == ("Windows.Win32.Foundation", "PWSTR")
  _ -> False

-- | A string the function only reads: an @[In] [Const] PWSTR@ parameter that
-- is NUL-terminated (not counted by another parameter, not a list of
-- strings). The wrapper takes it as any 'Windows.Core.WideString'. Gives
-- the constructor of @PWSTR@, and the parameter's name if it is optional.
wideParam :: Ctx -> MethodDef -> (Int, Type) -> Either String (Maybe (Text, Maybe Text))
wideParam ctx m (i, ty) = case (strip ty, find ((== i) . fromIntegral . paramSequence) (methodParams m)) of
  (TValueType r, Just p)
    | Right (TargetDef d) <- resolveTarget r
    , (typeDefNamespace d, typeDefName d) == ("Windows.Win32.Foundation", "PWSTR")
    , let fl = paramFlags p
    , hasFlag paramIn fl
    , not (hasFlag paramOut fl)
    , hasAttribute "ConstAttribute" p
    , not (any (`hasAttribute` p) ["NativeArrayInfoAttribute", "MemorySizeAttribute", "NotNullTerminatedAttribute", "NullNullTerminatedAttribute"]) -> do
        con <- typeName ctx d
        Right (Just (con, if hasFlag paramOptional fl then Just (paramName p) else Nothing))
  _ -> Right Nothing

-- | What a function's wrapper does with its result. The rules are those of
-- windows-rs 0.58 (@SignatureKind@ and @handle_last_error@ in its
-- @windows-bindgen@). windows-bindgen 0.59 to 0.66 keep them; later releases
-- no longer read win32metadata.
--
-- * @HRESULT@: checked with 'Windows.Core.check';
-- * @BOOL@ from a function that sets the last error: @FALSE@ throws
--   'Windows.Core.lastError';
-- * a handle from a function that sets the last error: an invalid handle
--   (an @InvalidHandleValue@, or null when the type names none) throws;
-- * an IID and an interface pointer that the function writes for it
--   (@riid@ and a @[ComOutPtr] void**@ that is not optional, as in
--   @CoCreateInstance@), for
--   functions returning @HRESULT@: the wrapper is generic in the interface,
--   passes its IID and returns it (@SignatureKind::Query@);
-- * a last parameter that is only an output (windows-rs's @is_retval@), for
--   functions returning @HRESULT@ or nothing: allocated by the wrapper and
--   returned.
--
-- Functions marked @CanReturnMultipleSuccessValues@ keep their result, as
-- do all others.
data Shape
  = Raw
  | CheckHResult
  | -- | The constructor of @BOOL@.
    CheckBool Text
  | -- | The handle's constructor, and the test for an invalid value of its
    -- field.
    CheckHandle Text (Text -> Text)
  | -- | Whether the result is an @HRESULT@ to check, and the type of the
    -- returned out parameter.
    ReturnOut Bool Text
  | -- | A returned out parameter that is a string Windows allocates: whether
    -- the result is an @HRESULT@ to check, the constructor of @PWSTR@, and
    -- the function that frees the string (from @FreeWith@).
    ReturnString Bool Text Text
  | -- | The positions of the IID parameter and of the interface pointer.
    Query Int Int

-- | Join Haddock blocks (a documentation link, notes on the declaration)
-- into one comment: the first block, a blank line, then the others as one
-- paragraph. A line that does not start a block continues the one before.
commentLines :: [Text] -> [Text]
commentLines = \case
  [] -> []
  l : ls -> l : go False ls
  where
    go _ [] = []
    go joined (x : xs) = case T.stripPrefix "-- | " x of
      Just rest -> ["--" | not joined] ++ ("-- " <> rest) : go True xs
      Nothing -> x : go joined xs

-- | What the wrapper adds to the C function, for its Haddock comment.
shapeComment :: Shape -> [Text]
shapeComment = \case
  Raw -> []
  CheckHResult -> ["-- | Throws 'Core.WindowsError' if the result is a failure."]
  CheckBool _ -> ["-- | Throws 'Core.WindowsError' from @GetLastError@ if the function fails."]
  CheckHandle {} -> ["-- | Throws 'Core.WindowsError' from @GetLastError@ if the handle is invalid."]
  ReturnOut True _ -> ["-- | Returns the last parameter. Throws 'Core.WindowsError' if the result is a failure."]
  ReturnOut False _ -> ["-- | Returns the last parameter."]
  ReturnString True _ free -> ["-- | Returns the string in the last parameter, and frees it with '" <> free <> "'. Throws 'Core.WindowsError' if the result is a failure."]
  ReturnString False _ free -> ["-- | Returns the string in the last parameter, and frees it with '" <> free <> "'."]
  Query {} -> ["-- | Returns the interface @t@ (choose it with a type application such as @\\@IStream@), which the caller releases. Throws 'Core.WindowsError' if the result is a failure."]

functionShape :: Ctx -> Map Text (Text, MethodDef) -> MethodDef -> Bool -> MethodSig -> Either String Shape
functionShape ctx frees m lastError sig
  | hasAttribute "CanReturnMultipleSuccessValuesAttribute" m = Right Raw
  | otherwise = case strip (sigReturnType sig) of
      TVoid | retval -> returned False
      TValueType r
        | Right (TargetDef d) <- resolveTarget r -> case () of
            _
              | coreType d == Just "HRESULT" -> case query of
                  Just (g, o) -> Right (Query g o)
                  Nothing -> if retval then returned True else Right CheckHResult
              | (typeDefNamespace d, typeDefName d) == ("Windows.Win32.Foundation", "BOOL")
              , lastError ->
                  CheckBool <$> typeName ctx d
              | classify d == KNativeTypedef
              , lastError
              -- LocalFree returns NULL when it succeeds and the handle when
              -- it fails, the reverse of the rule.
              , methodName m /= "LocalFree"
              , [f] <- instanceFields d
              , Just invalid <- invalidTest (strip (fieldType f)) (invalidValues d) ->
                  (`CheckHandle` invalid) <$> typeName ctx d
              | otherwise -> Right Raw
      _ -> Right Raw
  where
    rows = Map.fromList [(paramSequence p, p) | p <- methodParams m]
    flagsAt i = maybe 0 paramFlags (Map.lookup (fromIntegral i + 1) rows)
    -- windows-rs 0.58 @signature_param_is_query@: the last @GUID*@ that is
    -- not @[Out]@ (windows-rs makes every other pointer parameter const),
    -- and the last @void**@ marked @ComOutPtr@. When that one is optional
    -- (@QueryOptional@ there) there is no Query shape, and the wrapper only
    -- checks the @HRESULT@.
    query = case ( findIndices isGuidIn (zip [0 ..] (sigParams sig))
                 , findIndices isObject (zip [0 ..] (sigParams sig))
                 ) of
      (gs@(_ : _), os@(_ : _))
        | o <- last os
        , not (hasFlag paramOptional (flagsAt o)) ->
            Just (last gs, o)
      _ -> Nothing
    isGuidIn (i, t) = case strip t of
      TPtr g
        | TValueType r <- strip g ->
            resolveTarget r == Right TargetGuid && not (hasFlag paramOut (flagsAt (i :: Int)))
      _ -> False
    isObject (i, t) = case strip t of
      TPtr p
        | TPtr v <- strip p
        , TVoid <- strip v ->
            maybe False (hasAttribute "ComOutPtrAttribute") (Map.lookup (fromIntegral (i :: Int) + 1) rows)
      _ -> False
    n = length (sigParams sig)
    lastType = strip (last (sigParams sig))
    -- windows-rs 0.58 @is_retval@: win32metadata rarely uses @RetVal@, so
    -- a last parameter that is only written, is not optional or an array,
    -- and points to at most 16 bytes that are not a callback counts, when
    -- no other parameter is an output.
    retval =
      n > 0
        && case Map.lookup (fromIntegral n) rows of
          Just p | hasAttribute "RetValAttribute" p -> True
          Just p ->
            let fl = paramFlags p
             in hasFlag paramOut fl
                  && not (hasFlag paramIn fl)
                  && not (hasFlag paramOptional fl)
                  && not (hasAttribute "NativeArrayInfoAttribute" p)
                  && not (hasAttribute "MemorySizeAttribute" p)
                  && smallPointee
                  && and
                    [ not (hasFlag paramOut (paramFlags q))
                    | (i, q) <- Map.toList rows
                    , i > 0
                    , fromIntegral i < n
                    ]
          Nothing -> False
    smallPointee = case lastType of
      TPtr t
        | not (isVoid t)
        , not (isDelegate t)
        , Right (Layout size _) <- typeLayout t ->
            size <= 16
      _ -> False
    outType = case lastType of
      TPtr t -> hsType ctx t
      t -> Left ("return parameter of type " ++ T.unpack (renderType t))
    -- A string that Windows allocates is read and freed when the function
    -- that frees it takes just the pointer.
    returned hr = case (lastType, Map.lookup (fromIntegral n) rows >>= freeWith) of
      (TPtr (TValueType r), Just f)
        | Right (TargetDef d) <- resolveTarget r
        , (typeDefNamespace d, typeDefName d) == ("Windows.Win32.Foundation", "PWSTR")
        , Just (free, fm) <- Map.lookup f frees
        , let fs = methodSignature fm
        , isVoid (sigReturnType fs)
        , [TPtr _] <- map strip (sigParams fs) ->
            (\con -> ReturnString hr con free) <$> typeName ctx d
      _ -> ReturnOut hr <$> outType
    isVoid = \case
      TVoid -> True
      TPtr t -> isVoid t
      TModified _ _ t -> isVoid t
      _ -> False
    isDelegate = \case
      TClass r | Right (TargetDef d) <- resolveTarget r -> classify d == KDelegate
      TModified _ _ t -> isDelegate t
      _ -> False
    invalidValues d =
      [ v
      | Attribute {attrName = "InvalidHandleValueAttribute", attrArgs = [AttrValue x]} <- attributes d
      , Just v <- [integral x]
      ]
    invalidTest field values = case field of
      TPtr _
        | null values -> Just (\v -> v <> " P.== F.nullPtr")
        | otherwise -> Just (\v -> "P.elem (F.ptrToIntPtr " <> v <> ") " <> list values)
      _
        | null values -> Nothing
        | isIntegral field -> Just (\v -> "P.elem " <> v <> " " <> list values)
        | otherwise -> Nothing
    list vs = "[" <> T.intercalate ", " ["P.fromInteger (" <> tshow v <> ")" | v <- vs] <> "]"
    isIntegral = (`elem` [TInt8, TUInt8, TInt16, TUInt16, TInt32, TUInt32, TInt64, TUInt64, TIntPtr, TUIntPtr])
    integral = \case
      VInt8 x -> Just (toInteger x)
      VUInt8 x -> Just (toInteger x)
      VInt16 x -> Just (toInteger x)
      VUInt16 x -> Just (toInteger x)
      VInt32 x -> Just (toInteger x)
      VUInt32 x -> Just (toInteger x)
      VInt64 x -> Just (toInteger x)
      VUInt64 x -> Just (toInteger x)
      _ -> Nothing

strip :: Type -> Type
strip = \case
  TModified _ _ t -> strip t
  t -> t

------------------------------------------------------------------------------
-- Constants

emitConstant :: Ctx -> Text -> Field -> Either String Decl
emitConstant ctx name f = case (stringConstantType f, constantValue <$> fieldConstant f) of
  (Just tyName, Just (VString s)) -> emitString ctx name f tyName s
  _ -> do
    ty <- hsType ctx (fieldType f)
    lit <- constantLit ctx f
    let expr = litExpr lit
        pat = litPat lit
    Right
      Decl
        { declExports = ["pattern " <> name]
        , declBody =
            T.unlines $
              docComment f
                ++ ["pattern " <> name <> " :: " <> ty]
                ++ if pat == expr
                  then ["pattern " <> name <> " = " <> expr]
                  else
                    [ "pattern " <> name <> " <- " <> pat
                    , "  where"
                    , "    " <> name <> " = " <> expr
                    ]
        }

-- | A string constant. The string is allocated on first use by a
-- @NOINLINE@ value; the pattern matches that pointer, as comparing with the
-- constant does in C.
emitString :: Ctx -> Text -> Field -> Text -> Text -> Either String Decl
emitString ctx name f tyName s = do
  ty <- case [n | (t, n) <- Map.toList (ctxTypes ctx), typeDefNamespace t == "Windows.Win32.Foundation", typeDefName t == tyName] of
    n : _ -> Right n
    [] -> Left ("depends on Windows.Win32.Foundation." ++ T.unpack tyName ++ ", which is not emitted")
  let var = "str'" <> name
      helper = if tyName == "PSTR" then "Core.ansiLiteral" else "Core.wideLiteral"
  Right
    Decl
      { declExports = ["pattern " <> name]
      , declBody =
          T.unlines $
            docComment f
              ++ [ "pattern " <> name <> " :: " <> ty
                 , "pattern " <> name <> " <- ((P.== " <> var <> ") -> P.True)"
                 , "  where"
                 , "    " <> name <> " = " <> var
                 , ""
                 , var <> " :: " <> ty
                 , var <> " = " <> ty <> " (" <> helper <> " " <> T.pack (show (T.unpack s)) <> ")"
                 , "{-# NOINLINE " <> var <> " #-}"
                 ]
      }

-- | A constant value, built from Haskell literals so that it can be both
-- an expression and a pattern.
data Lit
  = -- | A numeric literal.
    LNum Text
  | -- | A pointer given as an integer, such as @HKEY_LOCAL_MACHINE@.
    LPtr Text
  | -- | A constructor applied to its fields.
    LCon Text [Lit]
  | LArr [Lit]

litExpr :: Lit -> Text
litExpr = \case
  LNum t -> t
  LPtr t -> "F.intPtrToPtr " <> t
  LCon c ls -> T.unwords (c : map (atom . litExpr) ls)
  LArr ls -> "FixedArray [" <> T.intercalate ", " (map litExpr ls) <> "]"

litPat :: Lit -> Text
litPat = \case
  LNum t -> t
  LPtr t -> "(F.ptrToIntPtr -> " <> t <> ")"
  LCon c ls -> T.unwords (c : map (atom . litPat) ls)
  LArr ls -> "FixedArray [" <> T.intercalate ", " (map litPat ls) <> "]"

-- | The value of a non-string constant. Numeric constants have a Constant
-- row; GUIDs are given by a @Guid@ attribute; other structs (@PROPERTYKEY@,
-- @SID_IDENTIFIER_AUTHORITY@) by a @Constant@ attribute holding a C
-- initializer such as @{{0x1234, ...}, 4}@.
constantLit :: Ctx -> Field -> Either String Lit
constantLit ctx f
  | Just c <- fieldConstant f = do
      v <- maybe (Left "unsupported constant value") Right (literal (constantValue c))
      scalarLit ctx (fieldType f) v
  | Just a <- findAttribute "GuidAttribute" f =
      maybe (Left "malformed Guid attribute") guidLit (mapM attrInteger (attrArgs a))
  | Just Attribute {attrArgs = [AttrString (Just s)]} <- findAttribute "ConstantAttribute" f = do
      inits <- parseInit s
      initLit ctx (fieldType f) inits
  | otherwise = Left "constant has no value"

attrInteger :: AttributeValue -> Maybe Integer
attrInteger = \case
  AttrValue v -> valueInteger v
  _ -> Nothing

-- | A numeric literal as a value of a numeric, pointer, enum or typedef type.
scalarLit :: Ctx -> Type -> Text -> Either String Lit
scalarLit ctx ty v = case ty of
  TModified _ _ t -> scalarLit ctx t v
  TPtr _ -> Right (LPtr v)
  TValueType r ->
    resolveTarget r >>= \case
      TargetDef d
        | classify d `elem` [KEnum, KNativeTypedef]
        , [inner] <- instanceFields d -> do
            n <- typeName ctx d
            LCon n . pure <$> scalarLit ctx (fieldType inner) v
      _ -> unsupported
  t
    | isNumeric t -> Right (LNum v)
    | otherwise -> unsupported
  where
    unsupported = Left ("constants of type " ++ T.unpack (renderType ty) ++ " are not supported")

guidLit :: [Integer] -> Either String Lit
guidLit = \case
  [a, b, c, d0, d1, d2, d3, d4, d5, d6, d7] ->
    Right (LCon "GUID" [hex a, hex b, hex c, LArr (map hex [d0, d1, d2, d3, d4, d5, d6, d7])])
  _ -> Left "a GUID needs 11 numbers"
  where
    hex x = LNum (T.pack ("0x" ++ showHex x ""))

-- | A C initializer: a number or a braced list.
data Init = INum Integer | IList [Init]

-- | Parse the comma-separated initializers of a @Constant@ attribute.
parseInit :: Text -> Either String [Init]
parseInit s = case items (T.strip s) of
  Just (xs, rest) | T.null rest -> Right xs
  _ -> Left ("cannot parse constant " ++ show s)
  where
    items t = item t >>= \(x, r) -> more [x] r
    more acc r = case T.uncons r of
      Just (',', r') -> item (T.stripStart r') >>= \(x, r'') -> more (x : acc) r''
      _ -> Just (reverse acc, r)
    item t = case T.uncons t of
      Just ('{', r) -> do
        (xs, r') <- items (T.stripStart r)
        case T.uncons r' of
          Just ('}', r'') -> Just (IList xs, T.stripStart r'')
          _ -> Nothing
      _ -> case TR.signed TR.decimal t of
        Right (n, r) -> Just (INum n, T.stripStart r)
        Left _ -> Nothing

-- | A whole @Constant@ attribute value as a value of the field's type. A
-- struct's fields are listed without braces; a lone @0@ zeroes a struct or
-- union, as @= {0}@ does in C.
initLit :: Ctx -> Type -> [Init] -> Either String Lit
initLit ctx ty inits = case (ty, inits) of
  (TModified _ _ t, _) -> initLit ctx t inits
  (TValueType r, _) ->
    resolveTarget r >>= \case
      TargetDef d
        | classify d == KStruct -> structLit ctx d inits
        | classify d == KUnion, [INum 0] <- inits -> do
            n <- typeName ctx d
            Layout size _ <- defLayout d
            Right (LCon n [LArr (replicate size (LNum "0"))])
      _ -> single
  _ -> single
  where
    single = case inits of
      [i] -> treeLit ctx ty i
      _ -> Left ("cannot use " ++ show (length inits) ++ " values for " ++ T.unpack (renderType ty))

structLit :: Ctx -> TypeDef -> [Init] -> Either String Lit
structLit ctx d inits = do
  n <- typeName ctx d
  let fields = instanceFields d
  case inits of
    [INum 0] | length fields /= 1 -> LCon n <$> mapM (\f -> zeroLit ctx (fieldType f)) fields
    _
      | length fields == length inits -> LCon n <$> sequence (zipWith (treeLit ctx . fieldType) fields inits)
      | otherwise -> Left ("wrong number of values for " ++ T.unpack (typeDefFullName d))

treeLit :: Ctx -> Type -> Init -> Either String Lit
treeLit ctx ty = \case
  INum v
    | isAggregate -> initLit ctx ty [INum v]
    | otherwise -> scalarLit ctx ty (number v)
  IList is -> case ty of
    TModified _ _ t -> treeLit ctx t (IList is)
    TArray e shape
      | product (arraySizes shape) == length is -> LArr <$> mapM (treeLit ctx e) is
    TValueType r ->
      resolveTarget r >>= \case
        TargetGuid -> mapM leaf is >>= guidLit
        TargetDef d | classify d == KStruct -> structLit ctx d is
        _ -> mismatch
    _ -> mismatch
  where
    isAggregate = case ty of
      TValueType r | Right (TargetDef d) <- resolveTarget r -> classify d `elem` [KStruct, KUnion]
      _ -> False
    leaf = \case
      INum v -> Right v
      IList _ -> mismatch
    mismatch :: Either String a
    mismatch = Left ("initializer does not match " ++ T.unpack (renderType ty))

-- | A zero value of a field's type, for a struct initialized with @{0}@.
zeroLit :: Ctx -> Type -> Either String Lit
zeroLit ctx ty = initLit ctx ty [INum 0]

number :: Integer -> Text
number v
  | v < 0 = "(" <> tshow v <> ")"
  | otherwise = tshow v

valueInteger :: Value -> Maybe Integer
valueInteger = \case
  VUInt8 x -> Just (toInteger x)
  VUInt16 x -> Just (toInteger x)
  VUInt32 x -> Just (toInteger x)
  VInt32 x -> Just (toInteger x)
  _ -> Nothing

isNumeric :: Type -> Bool
isNumeric = \case
  TModified _ _ t -> isNumeric t
  t ->
    t
      `elem` [ TBool
             , TChar
             , TInt8
             , TUInt8
             , TInt16
             , TUInt16
             , TInt32
             , TUInt32
             , TInt64
             , TUInt64
             , TFloat32
             , TFloat64
             , TIntPtr
             , TUIntPtr
             ]

-- | A Haskell literal for a constant, in parentheses when negative.
literal :: Value -> Maybe Text
literal = \case
  VBool b -> Just (if b then "1" else "0")
  VChar ch -> Just (tshow (fromEnum ch))
  VInt8 x -> signed x
  VUInt8 x -> Just (tshow x)
  VInt16 x -> signed x
  VUInt16 x -> Just (tshow x)
  VInt32 x -> signed x
  VUInt32 x -> Just (tshow x)
  VInt64 x -> signed x
  VUInt64 x -> Just (tshow x)
  VFloat32 x -> float x
  VFloat64 x -> float x
  VString _ -> Nothing
  VNull -> Nothing
  where
    signed :: (Show a, Ord a, Num a) => a -> Maybe Text
    signed x
      | x < 0 = Just ("(" <> tshow x <> ")")
      | otherwise = Just (tshow x)
    float :: (RealFloat a, Show a) => a -> Maybe Text
    float x
      | isNaN x || isInfinite x = Nothing
      | otherwise = signed x

------------------------------------------------------------------------------
-- Rendering

-- | A Haddock comment pointing at the item's documentation, if the metadata
-- has a link.
docComment :: (HasAttributes t) => Row t -> [Text]
docComment r = case findAttribute "DocumentationAttribute" r of
  Just Attribute {attrArgs = [AttrString (Just url)]} -> ["-- | <" <> url <> ">"]
  _ -> []

render :: Config -> [Decl] -> Text
render cfg decls =
  T.unlines $
    map ("-- " <>) (cfgHeader cfg)
      ++ [ "{-# LANGUAGE DataKinds #-}"
         , "{-# LANGUAGE DerivingStrategies #-}"
         , "{-# LANGUAGE DuplicateRecordFields #-}"
         , "{-# LANGUAGE FlexibleContexts #-}"
         , "{-# LANGUAGE FlexibleInstances #-}"
         , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
         , "{-# LANGUAGE MultiParamTypeClasses #-}"
         , -- Negative constants are written as (-2147483648); without this,
           -- that is negate 2147483648, which overflows Int32 and warns.
           "{-# LANGUAGE NegativeLiterals #-}"
         , "{-# LANGUAGE NoFieldSelectors #-}"
         , "{-# LANGUAGE NoImplicitPrelude #-}"
         , "{-# LANGUAGE PatternSynonyms #-}"
         , "{-# LANGUAGE TypeApplications #-}"
         , "{-# LANGUAGE ViewPatterns #-}"
         , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
         , ""
         , "module " <> cfgModuleName cfg
         ]
      ++ exportList (concatMap declExports decls)
      ++ [ ""
         , "import Data.Bits qualified as B"
         , "import Data.Int (Int16, Int32, Int64, Int8)"
         , "import Data.Word (Word16, Word32, Word64, Word8)"
         , "import Foreign.C.Types (CBool (..))"
         , "import Foreign.Marshal.Alloc qualified as M"
         , "import Foreign.Ptr (FunPtr, IntPtr (..), Ptr, WordPtr (..))"
         , "import Foreign.Ptr qualified as F"
         , "import Foreign.Storable qualified as S"
         , "import GHC.Records qualified as R"
         , "import Prelude (Double, Float, IO)"
         , "import Prelude qualified as P"
         , "import System.IO.Unsafe qualified as U"
         , "import Windows.Core (FixedArray (..), GUID (..), HRESULT (..), IUnknown (..))"
         , "import Windows.Core qualified as Core"
         ]
      ++ concat [["", T.stripEnd (declBody d)] | d <- decls, not (T.null (declBody d))]
  where
    exportList [] = ["  () where"]
    exportList (e : es) = ("  ( " <> e) : map ("  , " <>) es ++ ["  ) where"]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
