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
-- * a constant: a pattern synonym;
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
typeNames ts = Map.fromList (zip ts (dedupe reservedTypes (map rawName ts)))
  where
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
emitConstant ctx name f = do
  c <- maybe (Left "constant has no value (only numeric constants are supported)") Right (fieldConstant f)
  lit <- maybe (Left "unsupported constant value") Right (literal (constantValue c))
  (ty, expr) <- case fieldType f of
    TValueType r ->
      resolveTarget r >>= \case
        TargetDef d
          | classify d `elem` [KEnum, KNativeTypedef]
          , [inner] <- instanceFields d
          , isNumeric (fieldType inner) -> do
              n <- typeName ctx d
              Right (n, n <> " " <> lit)
        _ -> Left ("constants of type " ++ T.unpack (renderType (fieldType f)) ++ " are not supported")
    t
      | isNumeric t -> do
          hs <- hsType ctx t
          Right (hs, lit)
      | otherwise -> Left ("constants of type " ++ T.unpack (renderType t) ++ " are not supported")
  Right
    Decl
      { declExports = ["pattern " <> name]
      , declBody =
          T.unlines $
            docComment f
              ++ [ "pattern " <> name <> " :: " <> ty
                 , "pattern " <> name <> " = " <> expr
                 ]
      }

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
         , "import Windows.Core (FixedArray (..), GUID (..))"
         , "import Windows.Core qualified as Core"
         ]
      ++ concatMap (\d -> ["", T.stripEnd (declBody d)]) decls
  where
    exportList [] = ["  () where"]
    exportList (e : es) = ("  ( " <> e) : map ("  , " <>) es ++ ["  ) where"]

tshow :: (Show a) => a -> Text
tshow = T.pack . show
