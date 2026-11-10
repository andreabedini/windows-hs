{-# LANGUAGE CPP #-}

-- | Strings passed to and returned from Windows.
--
-- The W functions take UTF-16 strings (@PWSTR@), which Windows does not
-- validate: a file name can hold a lone surrogate. An 'OsString' (and so an
-- @OsPath@) holds exactly that on Windows, so it is passed without
-- conversion or loss.
--
-- A generated wrapper takes such a parameter as any 'WideString':
--
-- * an 'OsString' or @OsPath@, written @[osstr|...|]@ as a literal;
-- * the generated module's @PWSTR@, for its string constants (@WC_BUTTONW@),
--   resources given by number (@IDC_ARROW@, which C writes with
--   @MAKEINTRESOURCE@) and pointers from elsewhere;
-- * 'Nothing', for NULL where the parameter is optional, or 'Just' an
--   'OsString'.
--
-- Strings that Windows writes come back as 'OsString' too: those written
-- to a buffer the caller sizes ('withWideBuffer', 'peekWideBuffer') and
-- those Windows allocates for the caller to free ('takeWideString').
module Windows.Core.WideString
  ( WideString (..)
  , OsString
  , withWideBuffer
  , peekWideBuffer
  , peekWideString
  , takeWideString
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.Word (Word16)
import Foreign.Marshal.Array (allocaArray, lengthArray0)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peekElemOff)
import System.OsString.Data.ByteString.Short.Word16 qualified as W16
import System.OsString.Internal.Types (OsString (..), WindowsString (..))
#if !defined(mingw32_HOST_OS)
import System.OsString qualified as OsString
import System.OsString.Windows qualified as Windows
#endif

-- | Values that can be passed where Windows expects a NUL-terminated UTF-16
-- string it only reads.
class WideString a where
  -- | Run the action with a pointer to the string, valid until it returns.
  withWideString :: a -> (Ptr Word16 -> IO b) -> IO b

-- | Copied, with a NUL added.
instance WideString OsString where
#if defined(mingw32_HOST_OS)
  withWideString (OsString (WindowsString s)) = W16.useAsCWString s
#else
  -- Elsewhere an OsString is in the system's encoding, usually UTF-8. This
  -- only runs in tests: Windows functions cannot be called there.
  withWideString s k = do
    str <- OsString.decodeUtf s
    WindowsString w <- Windows.encodeUtf str
    W16.useAsCWString w k
#endif

-- | 'Nothing' is NULL. The equality makes a bare 'Nothing' an optional
-- 'OsString' rather than an ambiguous type.
instance (a ~ OsString) => WideString (Maybe a) where
  withWideString Nothing k = k nullPtr
  withWideString (Just s) k = withWideString s k

-- | Run the action with a zeroed buffer of @n@ UTF-16 units, valid until
-- it returns. For @n <= 0@ the buffer is NULL, which functions that report
-- the size they need accept.
withWideBuffer :: (Integral n) => n -> (Ptr Word16 -> IO a) -> IO a
withWideBuffer n k
  | len <= 0 = k nullPtr
  | otherwise = allocaArray len $ \p -> fillBytes p 0 (2 * len) >> k p
  where
    len = fromIntegral n :: Int

-- | The string in a buffer of @n@ UTF-16 units: up to its first NUL, or
-- all of it if there is none. Empty for a NULL buffer.
--
-- Windows functions differ in what they write when the buffer is too
-- small: some truncate, some write nothing and return the size they need.
-- The buffer from 'withWideBuffer' is zeroed, so it reads as empty when
-- nothing was written.
peekWideBuffer :: (Integral n) => Ptr Word16 -> n -> IO OsString
peekWideBuffer p n
  | p == nullPtr || len <= 0 = pure mempty
  | otherwise = findNul 0 >>= fromWide p
  where
    len = fromIntegral n :: Int
    findNul i
      | i >= len = pure len
      | otherwise = peekElemOff p i >>= \c -> if c == 0 then pure i else findNul (i + 1)

-- | A NUL-terminated string. Empty for NULL.
peekWideString :: Ptr Word16 -> IO OsString
peekWideString p
  | p == nullPtr = pure mempty
  | otherwise = lengthArray0 0 p >>= fromWide p

-- | @takeWideString p free check@ runs @check@, which throws if the call
-- that wrote @p@ failed, reads the NUL-terminated string @p@, and frees it
-- with @free@ whether or not either of them throws. A NULL @p@ is not
-- freed. For strings Windows allocates and the caller frees, such as
-- @SHGetKnownFolderPath@'s, which must be freed even when it fails.
takeWideString :: Ptr Word16 -> IO () -> IO () -> IO OsString
takeWideString p free check = (check >> peekWideString p) `finally` when (p /= nullPtr) free

-- | @n@ UTF-16 units.
fromWide :: Ptr Word16 -> Int -> IO OsString
#if defined(mingw32_HOST_OS)
fromWide p n = OsString . WindowsString <$> W16.packCWStringLen (p, n)
#else
-- Only in tests: see the 'OsString' instance of 'WideString'.
fromWide p n = do
  w <- W16.packCWStringLen (p, n)
  str <- Windows.decodeUtf (WindowsString w)
  OsString.encodeUtf str
#endif
