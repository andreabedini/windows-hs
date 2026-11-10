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
-- * a COM interface: an empty data type, used behind pointers;
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
import Data.List (sortOn)
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
    functionNames = dedupe (Set.fromList delegateMks) (map (valueName . methodName) functionList)
    (fnWarnings, fnDecls) =
      partitionEithers
        [ either (Left . warn (methodName m)) Right (emitFunction ctx n m)
        | (m, n) <- zip functionList functionNames
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
  TFnPtr sig -> ("FunPtr " <>) . atom <$> ffiFunctionType ctx sig
  TValueType r ->
    resolveTarget r >>= \case
      TargetGuid -> Right "GUID"
      TargetDef d -> typeName ctx d
  TClass r ->
    resolveTarget r >>= \case
      TargetDef d
        | classify d == KInterface -> ("Ptr " <>) <$> typeName ctx d
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
                     , "  peek p = " <> peekBody
                     , "  poke " <> (if null vars then "_" else "p") <> " (" <> T.unwords (name : vars) <> ") = " <> pokeBody
                     ]
          }
    KUnion -> do
      Layout size align <- defLayout t
      Right
        Decl
          { declExports = [name <> " (..)"]
          , declBody =
              T.unlines $
                doc
                  ++ [ "-- | A union or explicit-layout struct of " <> tshow size <> " bytes, kept as raw bytes."
                     , "newtype " <> name <> " = " <> name <> " (FixedArray " <> tshow size <> " Word8)"
                     , "  deriving stock (P.Eq, P.Ord, P.Show)"
                     , ""
                     , "instance S.Storable " <> name <> " where"
                     , "  sizeOf _ = " <> tshow size
                     , "  alignment _ = " <> tshow align
                     , "  peek p = " <> name <> " P.<$> S.peek (F.castPtr p)"
                     , "  poke p (" <> name <> " b) = S.poke (F.castPtr p) b"
                     ]
          }
    KDelegate -> do
      invoke <- maybe (Left "delegate without an Invoke method") Right (delegateInvoke t)
      fn <- ffiFunctionType ctx (methodSignature invoke)
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
    KInterface ->
      Right
        Decl
          { declExports = [name]
          , declBody = T.unlines (doc ++ ["data " <> name])
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
  where
    strip (TModified _ _ t) = strip t
    strip t = t

-- | The FFI type of a function pointer with this signature. Structs passed
-- by value appear as the integers or pointers the ABI uses.
ffiFunctionType :: Ctx -> MethodSig -> Either String Text
ffiFunctionType ctx sig = do
  checkSig sig
  params <- mapM (slot ctx) (sigParams sig)
  ret <- slot ctx (sigReturnType sig)
  let (extra, result) = case slotConv ret of
        ByRef -> ([slotFfi ret], slotFfi ret)
        _ -> ([], slotFfi ret)
  Right (T.intercalate " -> " (extra ++ map slotFfi params ++ ["IO " <> atom result]))

checkSig :: MethodSig -> Either String ()
checkSig sig = do
  when (sigCallingConvention sig == CallVararg || not (null (sigVarargParams sig))) $
    Left "variadic functions are not supported"
  when (sigGenericParamCount sig /= 0) $ Left "generic methods are not supported"

------------------------------------------------------------------------------
-- Functions

emitFunction :: Ctx -> Text -> MethodDef -> Either String Decl
emitFunction ctx name m = do
  im <- maybe (Left "no DllImport") Right (methodImplMap m)
  let sig = methodSignature m
  checkSig sig
  params <- mapM (slot ctx) (sigParams sig)
  ret <- slot ctx (sigReturnType sig)
  ffiType <- ffiFunctionType ctx sig
  let procName = "proc'" <> name
      callName = "call'" <> name
      args = ["a" <> tshow i | i <- [1 .. length params]]
      argIn a s = if slotConv s == Plain then a else a <> "'"
      callArgs = ["r" | slotConv ret == ByRef] ++ zipWith argIn args params
      call = T.unwords (callName : procName : callArgs)
      result = case slotConv ret of
        Plain -> call
        AsWord -> call <> " P.>>= Core.wordToStruct"
        ByRef -> "Core.returnStruct (\\r -> " <> call <> ")"
      wrappers =
        [ case slotConv s of
            AsWord -> "Core.structToWord " <> a <> " P.>>= \\" <> a <> "' ->"
            _ -> "Core.withStructCopy " <> a <> " P.$ \\" <> a <> "' ->"
        | (a, s) <- zip args params
        , slotConv s /= Plain
        ]
      hsSig = T.intercalate " -> " (map slotHs params ++ ["IO " <> atom (slotHs ret)])
      dll = moduleRefName (implMapScope im)
      entry = implMapImportName im
  unless (T.all (\c -> c /= '"' && c /= '\\') (dll <> entry)) $ Left "unexpected characters in DLL or entry point name"
  Right
    Decl
      { declExports = [name]
      , declBody =
          T.unlines $
            docComment m
              ++ [ name <> " :: " <> hsSig
                 , T.unwords (name : args) <> " ="
                 ]
              ++ map ("  " <>) (wrappers ++ [result])
              ++ [ ""
                 , procName <> " :: FunPtr (" <> ffiType <> ")"
                 , procName <> " = U.unsafePerformIO (Core.resolveProc \"" <> dll <> "\" \"" <> entry <> "\")"
                 , "{-# NOINLINE " <> procName <> " #-}"
                 , ""
                 , "foreign import ccall safe \"dynamic\""
                 , "  " <> callName <> " :: FunPtr (" <> ffiType <> ") -> " <> ffiType
                 ]
      }

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
  where
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
         , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
         , "{-# LANGUAGE NoFieldSelectors #-}"
         , "{-# LANGUAGE NoImplicitPrelude #-}"
         , "{-# LANGUAGE PatternSynonyms #-}"
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
         , "import Foreign.Ptr (FunPtr, IntPtr (..), Ptr, WordPtr (..))"
         , "import Foreign.Ptr qualified as F"
         , "import Foreign.Storable qualified as S"
         , "import Prelude (Double, Float, IO)"
         , "import Prelude qualified as P"
         , "import System.IO.Unsafe qualified as U"
         , "import Windows.Core (FixedArray (..), GUID (..), HRESULT (..))"
         , "import Windows.Core qualified as Core"
         ]
      ++ concat [["", T.stripEnd (declBody d)] | d <- decls, not (T.null (declBody d))]
  where
    exportList [] = ["  () where"]
    exportList (e : es) = ("  ( " <> e) : map ("  , " <>) es ++ ["  ) where"]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
