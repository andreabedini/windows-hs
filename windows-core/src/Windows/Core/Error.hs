{-# LANGUAGE CPP #-}

-- | Windows error codes and the exception that carries them.
--
-- As in windows-rs, every error is an 'HRESULT'. Functions that report
-- failure through @GetLastError@ return a Win32 error code, which
-- 'hresultFromWin32' turns into one.
--
-- Reading @GetLastError@ from Haskell after a generated function returns is
-- reliable: generated calls are @safe@, and GHC's runtime saves the
-- thread's last error when such a call returns and restores it whenever the
-- Haskell thread is rescheduled, possibly on another OS thread
-- (@resumeThread@ and @schedule@ in @rts/Schedule.c@).
module Windows.Core.Error
  ( module Windows.Core.HRESULT
  , windowsError
  , check
  , getLastError
  , lastError
  , throwLastError
  ) where

import Control.Exception (throwIO)
import Data.Bits ((.|.))
import Data.Char (isSpace)
import Data.Word (Word32)
import Foreign.C.String (CWString, peekCWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)
import Windows.Core.HRESULT
import Windows.Core.Link (resolveProc)

-- | The error for a code, with its description.
windowsError :: HRESULT -> IO WindowsError
windowsError hr = WindowsError hr <$> formatMessage hr

-- | Throw 'WindowsError' if the code is a failure; @HRESULT::ok@ in
-- windows-rs.
check :: HRESULT -> IO ()
check hr
  | failed hr = windowsError hr >>= throwIO
  | otherwise = pure ()

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "GetLastError"
  c_GetLastError :: IO Word32
#else
-- A stub elsewhere, as in "Windows.Core.Link".
c_GetLastError :: IO Word32
c_GetLastError = ioError (userError "GetLastError: Windows functions can only be called on Windows")
#endif

-- | The calling thread's last Win32 error code.
getLastError :: IO Word32
getLastError = c_GetLastError

-- | The error described by @GetLastError@; @Error::from_thread@ in
-- windows-rs. Call it straight after the function that failed.
lastError :: IO WindowsError
lastError = getLastError >>= windowsError . hresultFromWin32

-- | Throw 'lastError'.
throwLastError :: IO a
throwLastError = lastError >>= throwIO

------------------------------------------------------------------------------
-- Messages

type FormatMessageW = Word32 -> Ptr () -> Word32 -> Word32 -> Ptr CWString -> Word32 -> Ptr () -> IO Word32

foreign import ccall unsafe "dynamic"
  callFormatMessageW :: FunPtr FormatMessageW -> FormatMessageW

type LocalFree = Ptr () -> IO (Ptr ())

foreign import ccall unsafe "dynamic"
  callLocalFree :: FunPtr LocalFree -> LocalFree

-- Resolved like generated functions, so that only the bootstrap functions
-- are imported from kernel32 statically.
procFormatMessageW :: FunPtr FormatMessageW
procFormatMessageW = unsafePerformIO (resolveProc "KERNEL32.dll" "FormatMessageW")
{-# NOINLINE procFormatMessageW #-}

procLocalFree :: FunPtr LocalFree
procLocalFree = unsafePerformIO (resolveProc "KERNEL32.dll" "LocalFree")
{-# NOINLINE procLocalFree #-}

-- | The system's text for a code, without its trailing line break.
formatMessage :: HRESULT -> IO String
formatMessage (HRESULT c) = alloca $ \out -> do
  n <-
    callFormatMessageW
      procFormatMessageW
      (allocateBuffer .|. fromSystem .|. ignoreInserts)
      nullPtr
      (fromIntegral c)
      0
      out
      0
      nullPtr
  if n == 0
    then pure ""
    else do
      buf <- peek out
      s <- peekCWStringLen (buf, fromIntegral n)
      _ <- callLocalFree procLocalFree (castPtr buf)
      pure (reverse (dropWhile isSpace (reverse s)))
  where
    allocateBuffer = 0x100
    ignoreInserts = 0x200
    fromSystem = 0x1000
