-- | Signature blobs (ECMA-335 II.23.2): the types of fields, methods and
-- type specs.
module WinMD.Signature
  ( -- * Types
    Type (..)
  , ArrayShape (..)
  , MethodSig (..)
  , CallingConvention (..)

    -- * Accessors
  , fieldType
  , methodSignature
  , memberRefSignature
  , typeSpecType

    -- * Decoders
  , decodeFieldSig
  , decodeMethodSig
  , decodeTypeSpec
  , typeParser
  , renderType
  ) where

import Control.Monad (replicateM, unless)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import WinMD.Internal.Database (Database, KnownTable, throwWinMD)
import WinMD.Internal.Parser
import WinMD.Tables

-- | A type in a signature (II.23.2.12).
data Type
  = TVoid
  | TBool
  | TChar
  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TInt64
  | TUInt64
  | TFloat32
  | TFloat64
  | TIntPtr
  | TUIntPtr
  | TString
  | TObject
  | TTypedByRef
  | TPtr Type
  | TByRef Type
  | TPinned Type
  | TValueType TypeDefOrRef
  | TClass TypeDefOrRef
  | TSZArray Type
  | TArray Type ArrayShape
  | -- | A generic instantiation: the generic type and its arguments.
    TGenericInst Type [Type]
  | -- | A generic parameter of the enclosing type.
    TVar Int
  | -- | A generic parameter of the enclosing method.
    TMVar Int
  | TFnPtr MethodSig
  | -- | A custom modifier: whether it is required (@modreq@), the modifier
    -- type, and the modified type.
    TModified Bool TypeDefOrRef Type
  deriving stock (Eq, Show)

-- | The shape of a general array (II.23.2.13).
data ArrayShape = ArrayShape
  { arrayRank :: !Int
  , arraySizes :: [Int]
  , arrayLowerBounds :: [Int]
  }
  deriving stock (Eq, Show)

data CallingConvention
  = CallDefault
  | CallC
  | CallStdcall
  | CallThiscall
  | CallFastcall
  | CallVararg
  | CallUnmanaged
  deriving stock (Eq, Show)

-- | A method signature (II.23.2.1, II.23.2.2, II.23.2.3).
data MethodSig = MethodSig
  { sigCallingConvention :: !CallingConvention
  , sigHasThis :: !Bool
  , sigExplicitThis :: !Bool
  , sigGenericParamCount :: !Int
  , sigReturnType :: Type
  , sigParams :: [Type]
  , sigVarargParams :: [Type]
  -- ^ Parameters after the vararg sentinel (call-site signatures only).
  }
  deriving stock (Eq, Show)

------------------------------------------------------------------------------
-- Accessors

-- | The type of a field. Throws 'WinMDError' if the signature is malformed.
fieldType :: Field -> Type
fieldType r = orThrow r (decodeFieldSig (rowDatabase r) (fieldSignatureBlob r))

-- | The signature of a method. Throws 'WinMDError' if it is malformed.
methodSignature :: MethodDef -> MethodSig
methodSignature r = orThrow r (decodeMethodSig (rowDatabase r) (methodSignatureBlob r))

-- | The method signature of a member reference. Throws 'WinMDError' if it is
-- malformed or is a field signature.
memberRefSignature :: MemberRef -> MethodSig
memberRefSignature r = orThrow r (decodeMethodSig (rowDatabase r) (memberRefSignatureBlob r))

-- | The type described by a type spec. Throws 'WinMDError' if it is
-- malformed.
typeSpecType :: TypeSpec -> Type
typeSpecType r = orThrow r (decodeTypeSpec (rowDatabase r) (typeSpecSignatureBlob r))

orThrow :: (KnownTable t) => Row t -> Either String a -> a
orThrow r = either (\e -> throwWinMD (show r ++ " signature: " ++ e)) id

------------------------------------------------------------------------------
-- Decoders

-- | Decode a field signature (II.23.2.4).
decodeFieldSig :: Database -> ByteString -> Either String Type
decodeFieldSig db = runParser $ do
  b <- u8
  unless (b .&. 0x0F == 0x06) $ parseError ("not a field signature (lead byte " ++ show b ++ ")")
  typeParser db

-- | Decode a method signature (II.23.2.1-3).
decodeMethodSig :: Database -> ByteString -> Either String MethodSig
decodeMethodSig db = runParser (methodSigParser db)

-- | Decode a type spec signature (II.23.2.14).
decodeTypeSpec :: Database -> ByteString -> Either String Type
decodeTypeSpec db = runParser (typeParser db)

methodSigParser :: Database -> Parser MethodSig
methodSigParser db = do
  b <- u8
  conv <- case b .&. 0x0F of
    0x0 -> pure CallDefault
    0x1 -> pure CallC
    0x2 -> pure CallStdcall
    0x3 -> pure CallThiscall
    0x4 -> pure CallFastcall
    0x5 -> pure CallVararg
    0x9 -> pure CallUnmanaged
    k -> parseError ("not a method signature (calling convention " ++ show k ++ ")")
  generic <-
    if b .&. 0x10 /= 0
      then fromIntegral <$> compressedU32
      else pure 0
  n <- fromIntegral <$> compressedU32
  ret <- typeParser db
  (params, varargs) <- paramsParser n
  pure
    MethodSig
      { sigCallingConvention = conv
      , sigHasThis = b .&. 0x20 /= 0
      , sigExplicitThis = b .&. 0x40 /= 0
      , sigGenericParamCount = generic
      , sigReturnType = ret
      , sigParams = params
      , sigVarargParams = varargs
      }
  where
    paramsParser :: Int -> Parser ([Type], [Type])
    paramsParser 0 = pure ([], [])
    paramsParser n = do
      b <- peekU8
      if b == elemSentinel
        then do
          _ <- u8
          rest <- replicateM n (typeParser db)
          pure ([], rest)
        else do
          t <- typeParser db
          (ps, vs) <- paramsParser (n - 1)
          pure (t : ps, vs)

elemSentinel :: Word8
elemSentinel = 0x41

-- | Parse a type, including any leading custom modifiers, @BYREF@ and
-- @PINNED@ markers.
typeParser :: Database -> Parser Type
typeParser db = go
  where
    go = do
      e <- u8
      case e of
        0x01 -> pure TVoid
        0x02 -> pure TBool
        0x03 -> pure TChar
        0x04 -> pure TInt8
        0x05 -> pure TUInt8
        0x06 -> pure TInt16
        0x07 -> pure TUInt16
        0x08 -> pure TInt32
        0x09 -> pure TUInt32
        0x0A -> pure TInt64
        0x0B -> pure TUInt64
        0x0C -> pure TFloat32
        0x0D -> pure TFloat64
        0x0E -> pure TString
        0x0F -> TPtr <$> go
        0x10 -> TByRef <$> go
        0x11 -> TValueType <$> typeDefOrRefP
        0x12 -> TClass <$> typeDefOrRefP
        0x13 -> TVar . fromIntegral <$> compressedU32
        0x14 -> TArray <$> go <*> arrayShape
        0x15 -> do
          base <- go
          n <- fromIntegral <$> compressedU32
          TGenericInst base <$> replicateM n go
        0x16 -> pure TTypedByRef
        0x18 -> pure TIntPtr
        0x19 -> pure TUIntPtr
        0x1B -> TFnPtr <$> methodSigParser db
        0x1C -> pure TObject
        0x1D -> TSZArray <$> go
        0x1E -> TMVar . fromIntegral <$> compressedU32
        0x1F -> TModified True <$> typeDefOrRefP <*> go
        0x20 -> TModified False <$> typeDefOrRefP <*> go
        0x45 -> TPinned <$> go
        _ -> parseError ("unknown element type " ++ show e)

    typeDefOrRefP = do
      v <- compressedU32
      either parseError pure (typeDefOrRefFromEncoded db v)

    arrayShape = do
      rank <- fromIntegral <$> compressedU32
      numSizes <- fromIntegral <$> compressedU32
      sizes <- replicateM numSizes (fromIntegral <$> compressedU32)
      numBounds <- fromIntegral <$> compressedU32
      bounds <- replicateM numBounds (fromIntegral <$> compressedI32)
      pure (ArrayShape rank sizes bounds)

------------------------------------------------------------------------------
-- Rendering

-- | Render a type in a C#-like notation, for diagnostics.
renderType :: Type -> Text
renderType = \case
  TVoid -> "void"
  TBool -> "bool"
  TChar -> "char"
  TInt8 -> "sbyte"
  TUInt8 -> "byte"
  TInt16 -> "short"
  TUInt16 -> "ushort"
  TInt32 -> "int"
  TUInt32 -> "uint"
  TInt64 -> "long"
  TUInt64 -> "ulong"
  TFloat32 -> "float"
  TFloat64 -> "double"
  TIntPtr -> "nint"
  TUIntPtr -> "nuint"
  TString -> "string"
  TObject -> "object"
  TTypedByRef -> "typedref"
  TPtr t -> renderType t <> "*"
  TByRef t -> "ref " <> renderType t
  TPinned t -> "pinned " <> renderType t
  TValueType t -> name t
  TClass t -> name t
  TSZArray t -> renderType t <> "[]"
  TArray t s -> renderType t <> "[" <> T.replicate (arrayRank s - 1) "," <> "]"
  TGenericInst t args -> renderType t <> "<" <> T.intercalate ", " (map renderType args) <> ">"
  TVar i -> "!" <> T.pack (show i)
  TMVar i -> "!!" <> T.pack (show i)
  TFnPtr s ->
    "delegate*<"
      <> T.intercalate ", " (map renderType (sigParams s ++ [sigReturnType s]))
      <> ">"
  TModified req m t ->
    renderType t <> (if req then " modreq(" else " modopt(") <> name m <> ")"
  where
    name t = case typeDefOrRefName t of
      Just (ns, n)
        | T.null ns -> n
        | otherwise -> ns <> "." <> n
      Nothing -> case t of
        IsTypeSpec s -> renderType (typeSpecType s)
        _ -> "?"
