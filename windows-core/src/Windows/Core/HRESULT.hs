-- | Windows status codes and the exception that carries them. This module
-- calls no Windows functions; "Windows.Core.Error" adds the ones that do.
module Windows.Core.HRESULT
  ( HRESULT (..)
  , succeeded
  , failed
  , hresultFromWin32
  , WindowsError (..)
  ) where

import Control.Exception (Exception (..))
import Data.Bits ((.&.), (.|.))
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Storable (Storable)
import Numeric (showHex)

-- | A Windows status code: negative for failure, zero or positive for
-- success. Generated bindings use this type for
-- @Windows.Win32.Foundation.HRESULT@.
newtype HRESULT = HRESULT Int32
  deriving stock (Show)
  deriving newtype (Eq, Ord, Storable)

succeeded :: HRESULT -> Bool
succeeded (HRESULT c) = c >= 0

failed :: HRESULT -> Bool
failed = not . succeeded

-- | The C macro @HRESULT_FROM_WIN32@: the code in the @FACILITY_WIN32@
-- facility, marked as a failure. 0 (@ERROR_SUCCESS@) stays 0.
hresultFromWin32 :: Word32 -> HRESULT
hresultFromWin32 e
  | fromIntegral e <= (0 :: Int32) = HRESULT (fromIntegral e)
  | otherwise = HRESULT (fromIntegral ((e .&. 0xFFFF) .|. 0x80070000))

-- | A failed 'HRESULT' with the system's description of it.
data WindowsError = WindowsError
  { errorCode :: !HRESULT
  , errorMessage :: String
  -- ^ From @FormatMessageW@; empty if the system has no text for the code.
  }
  deriving stock (Eq)

instance Show WindowsError where
  showsPrec d (WindowsError (HRESULT c) msg) =
    showParen (d > 10) $
      showString "WindowsError 0x"
        . showString (pad (showHex (fromIntegral c :: Word32) ""))
        . (if null msg then id else showString ": " . showString msg)
    where
      pad s = replicate (8 - length s) '0' ++ s

instance Exception WindowsError where
  displayException = show
