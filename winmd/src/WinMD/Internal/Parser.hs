-- | A minimal bounds-checked parser over strict 'ByteString's, used for the
-- PE container, the metadata root and signature and attribute blobs. All
-- multi-byte integers in ECMA-335 are little-endian.
module WinMD.Internal.Parser
  ( Parser
  , runParser
  , runParserAt
  , parseError
  , u8
  , u16
  , u32
  , u64
  , peekU8
  , bytes
  , skip
  , seek
  , align
  , position
  , remaining
  , compressedU32
  , compressedI32
  , le16
  , le32
  , le64
  , decompress
  ) where

import Control.Monad (ap)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BU
import Data.Int (Int32)
import Data.Word (Word16, Word32, Word64, Word8)

newtype Parser a = Parser {unParser :: ByteString -> Int -> Either String (a, Int)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \bs i -> case p bs i of
    Left e -> Left e
    Right (a, j) -> Right (f a, j)

instance Applicative Parser where
  pure a = Parser $ \_ i -> Right (a, i)
  (<*>) = ap

instance Monad Parser where
  Parser p >>= k = Parser $ \bs i -> case p bs i of
    Left e -> Left e
    Right (a, j) -> unParser (k a) bs j

instance MonadFail Parser where
  fail = parseError

-- | Run a parser from the start of the input.
runParser :: Parser a -> ByteString -> Either String a
runParser p bs = runParserAt p bs 0

-- | Run a parser from the given offset.
runParserAt :: Parser a -> ByteString -> Int -> Either String a
runParserAt p bs i = fst <$> unParser p bs i

parseError :: String -> Parser a
parseError msg = Parser $ \_ i -> Left (msg ++ " (at offset " ++ show i ++ ")")

need :: Int -> Parser ()
need n = Parser $ \bs i ->
  if n >= 0 && i + n <= BS.length bs
    then Right ((), i)
    else
      Left
        ( "unexpected end of data: need "
            ++ show n
            ++ " bytes at offset "
            ++ show i
            ++ ", "
            ++ show (BS.length bs - i)
            ++ " available"
        )

u8 :: Parser Word8
u8 = need 1 >> Parser (\bs i -> Right (BU.unsafeIndex bs i, i + 1))

peekU8 :: Parser Word8
peekU8 = need 1 >> Parser (\bs i -> Right (BU.unsafeIndex bs i, i))

u16 :: Parser Word16
u16 = need 2 >> Parser (\bs i -> Right (le16 bs i, i + 2))

u32 :: Parser Word32
u32 = need 4 >> Parser (\bs i -> Right (le32 bs i, i + 4))

u64 :: Parser Word64
u64 = need 8 >> Parser (\bs i -> Right (le64 bs i, i + 8))

bytes :: Int -> Parser ByteString
bytes n = need n >> Parser (\bs i -> Right (BU.unsafeTake n (BU.unsafeDrop i bs), i + n))

skip :: Int -> Parser ()
skip n = need n >> Parser (\_ i -> Right ((), i + n))

-- | Move to an absolute offset. Seeking to the end of the input is allowed.
seek :: Int -> Parser ()
seek o = Parser $ \bs _ ->
  if o >= 0 && o <= BS.length bs
    then Right ((), o)
    else Left ("seek to offset " ++ show o ++ " outside data of length " ++ show (BS.length bs))

-- | Advance to the next multiple of @n@.
align :: Int -> Parser ()
align n = position >>= \p -> seek (((p + n - 1) `div` n) * n)

position :: Parser Int
position = Parser $ \_ i -> Right (i, i)

remaining :: Parser Int
remaining = Parser $ \bs i -> Right (BS.length bs - i, i)

-- | A compressed unsigned integer (ECMA-335 II.23.2).
compressedU32 :: Parser Word32
compressedU32 = do
  b0 <- peekU8
  case decompressLength b0 of
    Nothing -> parseError ("invalid compressed integer lead byte " ++ show b0)
    Just n -> decompress <$> bytes n

-- | A compressed signed integer (ECMA-335 II.23.2). The value is stored
-- rotated left by one bit, with the sign in the low bit.
compressedI32 :: Parser Int32
compressedI32 = do
  b0 <- peekU8
  n <- maybe (parseError ("invalid compressed integer lead byte " ++ show b0)) pure (decompressLength b0)
  u <- decompress <$> bytes n
  let bits = case n of
        1 -> 7
        2 -> 14
        _ -> 29 :: Int
      v = fromIntegral (u `shiftR` 1) :: Int32
  pure $
    if u .&. 1 == 0
      then v
      else v - (1 `shiftL` (bits - 1))

decompressLength :: Word8 -> Maybe Int
decompressLength b
  | b .&. 0x80 == 0 = Just 1
  | b .&. 0xC0 == 0x80 = Just 2
  | b .&. 0xE0 == 0xC0 = Just 4
  | otherwise = Nothing

-- | Decode a compressed unsigned integer whose bytes are given exactly.
decompress :: ByteString -> Word32
decompress bs = case BS.unpack bs of
  [b0] -> fromIntegral b0
  [b0, b1] -> (fromIntegral (b0 .&. 0x3F) `shiftL` 8) .|. fromIntegral b1
  [b0, b1, b2, b3] ->
    (fromIntegral (b0 .&. 0x1F) `shiftL` 24)
      .|. (fromIntegral b1 `shiftL` 16)
      .|. (fromIntegral b2 `shiftL` 8)
      .|. fromIntegral b3
  _ -> 0

-- | Little-endian reads without bounds checks. Callers must have validated
-- the offset.
le16 :: ByteString -> Int -> Word16
le16 bs i =
  fromIntegral (BU.unsafeIndex bs i)
    .|. (fromIntegral (BU.unsafeIndex bs (i + 1)) `shiftL` 8)

le32 :: ByteString -> Int -> Word32
le32 bs i =
  fromIntegral (le16 bs i) .|. (fromIntegral (le16 bs (i + 2)) `shiftL` 16)

le64 :: ByteString -> Int -> Word64
le64 bs i =
  fromIntegral (le32 bs i) .|. (fromIntegral (le32 bs (i + 4)) `shiftL` 32)
