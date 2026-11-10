-- | Custom attribute values (ECMA-335 II.23.3).
--
-- win32metadata records much of what C headers leave implicit as custom
-- attributes: @NativeTypedefAttribute@, @SupportedArchitectureAttribute@,
-- @NativeArrayInfoAttribute@, @ConstAttribute@ and so on.
module WinMD.Attribute
  ( Attribute (..)
  , AttributeValue (..)
  , attributes
  , findAttribute
  , hasAttribute
  , decodeAttribute
  , decodeAttributeEither
  ) where

import Control.Monad (replicateM, unless)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import WinMD.Internal.Database (Database, throwWinMD)
import WinMD.Internal.Parser
import WinMD.Signature
import WinMD.Tables

-- | A decoded custom attribute.
data Attribute = Attribute
  { attrNamespace :: Text
  , attrName :: Text
  -- ^ The attribute type's name, e.g. @NativeTypedefAttribute@.
  , attrArgs :: [AttributeValue]
  -- ^ Constructor arguments, in order.
  , attrNamedArgs :: [(Text, AttributeValue)]
  -- ^ Named field and property arguments.
  }
  deriving stock (Eq, Show)

data AttributeValue
  = -- | A primitive value.
    AttrValue Value
  | AttrString (Maybe Text)
  | -- | A @System.Type@ argument, as its serialized name.
    AttrType (Maybe Text)
  | -- | An enum value: the enum's full name and its underlying value.
    AttrEnum Text Value
  | AttrArray (Maybe [AttributeValue])
  deriving stock (Eq, Show)

-- | All attributes of a row, decoded. Throws 'WinMDError' on a malformed
-- attribute.
attributes :: (HasAttributes t) => Row t -> [Attribute]
attributes = map decodeAttribute . customAttributes

-- | The first attribute with the given type name (e.g.
-- @"NativeTypedefAttribute"@). Only that attribute is decoded.
findAttribute :: (HasAttributes t) => Text -> Row t -> Maybe Attribute
findAttribute name r =
  listToMaybe
    [ decodeAttribute a
    | a <- customAttributes r
    , fmap snd (customAttributeTypeName a) == Just name
    ]

hasAttribute :: (HasAttributes t) => Text -> Row t -> Bool
hasAttribute name r =
  any (\a -> fmap snd (customAttributeTypeName a) == Just name) (customAttributes r)

-- | Decode an attribute. Throws 'WinMDError' if it is malformed.
decodeAttribute :: CustomAttribute -> Attribute
decodeAttribute r = either (\e -> throwWinMD (show r ++ ": " ++ e)) id (decodeAttributeEither r)

decodeAttributeEither :: CustomAttribute -> Either String Attribute
decodeAttributeEither r = do
  (ns, name) <- maybe (Left "cannot resolve the attribute type") Right (customAttributeTypeName r)
  ctorBlob <- case customAttributeCtor r of
    Just (CtorMethodDef m) -> Right (methodSignatureBlob m)
    Just (CtorMemberRef m) -> Right (memberRefSignatureBlob m)
    Nothing -> Left "attribute has no constructor"
  sig <- decodeMethodSig db ctorBlob
  (args, named) <- runParser (valueParser db sig) (customAttributeValueBlob r)
  pure (Attribute ns name args named)
  where
    db = rowDatabase r

valueParser :: Database -> MethodSig -> Parser ([AttributeValue], [(Text, AttributeValue)])
valueParser db sig = do
  prolog <- u16
  unless (prolog == 0x0001) $ parseError ("bad custom attribute prolog " ++ show prolog)
  args <- mapM (fixedArg db) (sigParams sig)
  n <- remaining
  named <-
    if n == 0
      then pure []
      else do
        count <- u16
        replicateM (fromIntegral count) (namedArg db)
  pure (args, named)

fixedArg :: Database -> Type -> Parser AttributeValue
fixedArg db = \case
  TString -> AttrString <$> serString
  TClass t
    | typeDefOrRefName t == Just ("System", "Type") -> AttrType <$> serString
  -- II.23.3: the only value types a constructor argument can have are
  -- enums.
  TValueType t -> do
    let (name, base) = enumOfRef db t
    AttrEnum name <$> primitive base
  TSZArray e -> do
    n <- u32
    if n == 0xFFFFFFFF
      then pure (AttrArray Nothing)
      else AttrArray . Just <$> replicateM (fromIntegral n) (fixedArg db e)
  TObject -> fieldOrPropType >>= elemArg db
  TModified _ _ t -> fixedArg db t
  t -> AttrValue <$> primitive t

-- | The types a named argument or boxed value can declare (II.23.3).
data ElemType
  = ElemPrim Type
  | ElemType
  | ElemBoxed
  | ElemEnum Text
  | ElemArray ElemType

fieldOrPropType :: Parser ElemType
fieldOrPropType = do
  b <- u8
  case b of
    0x1D -> ElemArray <$> fieldOrPropType
    0x50 -> pure ElemType
    0x51 -> pure ElemBoxed
    0x55 -> ElemEnum . fromMaybe "" <$> serString
    _ -> case lookup b primitiveCodes of
      Just t -> pure (ElemPrim t)
      Nothing -> parseError ("unknown attribute element type " ++ show b)

elemArg :: Database -> ElemType -> Parser AttributeValue
elemArg db = \case
  ElemPrim TString -> AttrString <$> serString
  ElemPrim t -> AttrValue <$> primitive t
  ElemType -> AttrType <$> serString
  ElemBoxed -> fieldOrPropType >>= elemArg db
  ElemEnum name -> AttrEnum (enumTypeName name) <$> primitive (enumOfName db name)
  ElemArray e -> do
    n <- u32
    if n == 0xFFFFFFFF
      then pure (AttrArray Nothing)
      else AttrArray . Just <$> replicateM (fromIntegral n) (elemArg db e)

namedArg :: Database -> Parser (Text, AttributeValue)
namedArg db = do
  kind <- u8
  unless (kind == 0x53 || kind == 0x54) $ parseError ("bad named argument kind " ++ show kind)
  ty <- fieldOrPropType
  name <- fromMaybe "" <$> serString
  v <- elemArg db ty
  pure (name, v)

primitiveCodes :: [(Word8, Type)]
primitiveCodes =
  [ (0x02, TBool)
  , (0x03, TChar)
  , (0x04, TInt8)
  , (0x05, TUInt8)
  , (0x06, TInt16)
  , (0x07, TUInt16)
  , (0x08, TInt32)
  , (0x09, TUInt32)
  , (0x0A, TInt64)
  , (0x0B, TUInt64)
  , (0x0C, TFloat32)
  , (0x0D, TFloat64)
  , (0x0E, TString)
  ]

primitive :: Type -> Parser Value
primitive = \case
  TBool -> VBool . (/= 0) <$> u8
  TChar -> VChar . toEnum . fromIntegral <$> u16
  TInt8 -> VInt8 . fromIntegral <$> u8
  TUInt8 -> VUInt8 <$> u8
  TInt16 -> VInt16 . fromIntegral <$> u16
  TUInt16 -> VUInt16 <$> u16
  TInt32 -> VInt32 . fromIntegral <$> u32
  TUInt32 -> VUInt32 <$> u32
  TInt64 -> VInt64 . fromIntegral <$> u64
  TUInt64 -> VUInt64 <$> u64
  TFloat32 -> VFloat32 . castWord32ToFloat <$> u32
  TFloat64 -> VFloat64 . castWord64ToDouble <$> u64
  TIntPtr -> VInt64 . fromIntegral <$> u64
  TUIntPtr -> VUInt64 <$> u64
  t -> parseError ("unsupported attribute argument type " ++ T.unpack (renderType t))

-- | A SerString: a compressed length and UTF-8 bytes, or 0xFF for null.
serString :: Parser (Maybe Text)
serString = do
  b <- peekU8
  if b == 0xFF
    then u8 >> pure Nothing
    else do
      n <- compressedU32
      Just . decodeUtf8Lenient <$> bytes (fromIntegral n)

-- | The full name and underlying type of an enum used as an attribute
-- argument. Enums defined outside this file (in @System@) are assumed to be
-- 32-bit, as all the ones attribute constructors use are.
enumOfRef :: Database -> TypeDefOrRef -> (Text, Type)
enumOfRef db t = case t of
  IsTypeDef d -> (typeDefFullName d, underlying d)
  IsTypeRef r ->
    let ns = typeRefNamespace r
        n = typeRefName r
     in (dotted ns n, maybe TInt32 underlying (findTypeDef db ns n))
  IsTypeSpec _ -> ("?", TInt32)

-- | The underlying type of an enum named in a serialized attribute value,
-- e.g. @"Windows.Win32.Foundation.Metadata.Architecture, Windows.Win32"@.
enumOfName :: Database -> Text -> Type
enumOfName db name =
  let (ns, n) = splitName (enumTypeName name)
   in maybe TInt32 underlying (findTypeDef db ns n)

enumTypeName :: Text -> Text
enumTypeName = T.strip . T.takeWhile (/= ',')

splitName :: Text -> (Text, Text)
splitName full = case T.breakOnEnd "." full of
  ("", n) -> ("", n)
  (ns, n) -> (T.dropEnd 1 ns, n)

dotted :: Text -> Text -> Text
dotted ns n
  | T.null ns = n
  | otherwise = ns <> "." <> n

-- | An enum's underlying type is the type of its instance field
-- (@value__@).
underlying :: TypeDef -> Type
underlying d =
  case [fieldType f | f <- typeDefFields d, not (hasFlag fieldStatic (fieldFlags f))] of
    t : _ -> t
    [] -> TInt32
