-- | Storage for the string constants in generated code.
--
-- A generated string constant is a top-level @NOINLINE@ value that calls
-- one of these functions once, so the string is encoded and allocated the
-- first time it is used and then shared. The memory is never freed: like a
-- string literal in C, it lives as long as the program.
module Windows.Core.Literal
  ( wideLiteral
  , ansiLiteral
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.Word (Word16, Word8)
import Foreign.Marshal.Array (newArray0)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Foreign qualified as GHC
import System.IO (utf8)
import System.IO.Unsafe (unsafePerformIO)

-- | A NUL-terminated UTF-16 copy of a string, for @PWSTR@ constants.
wideLiteral :: String -> Ptr Word16
wideLiteral s = unsafePerformIO (newArray0 0 (concatMap utf16 s))
{-# NOINLINE wideLiteral #-}

-- | A NUL-terminated UTF-8 copy of a string, for @PSTR@ constants. The
-- strings in win32metadata are ASCII; windows-rs encodes them as UTF-8 too.
ansiLiteral :: String -> Ptr Word8
ansiLiteral s = unsafePerformIO (castPtr <$> GHC.newCString utf8 s)
{-# NOINLINE ansiLiteral #-}

-- | The UTF-16 code units of a character.
utf16 :: Char -> [Word16]
utf16 c
  | n < 0x10000 = [fromIntegral n]
  | otherwise =
      let m = n - 0x10000
       in [0xD800 + fromIntegral (m `shiftR` 10), 0xDC00 + fromIntegral (m .&. 0x3FF)]
  where
    n = ord c
