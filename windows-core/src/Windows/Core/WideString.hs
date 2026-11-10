{-# LANGUAGE CPP #-}

-- | Read-only string parameters.
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
module Windows.Core.WideString
  ( WideString (..)
  ) where

import Data.Word (Word16)
import Foreign.Ptr (Ptr, nullPtr)
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
