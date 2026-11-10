{-# LANGUAGE CPP #-}

-- | Resolve Win32 functions at runtime.
--
-- Generated bindings never link against import libraries. Each function is
-- looked up with @LoadLibraryExW@ and @GetProcAddress@ the first time it is
-- called, and the pointer is kept for the life of the process.
module Windows.Core.Link
  ( resolveProc
  , LinkError (..)
  ) where

import Control.Exception (Exception, throwIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32)
import Foreign.C.String (CString, CWString, withCString, withCWString)
import Foreign.Ptr (FunPtr, Ptr, nullFunPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

-- | A DLL or one of its exports could not be found. The 'Word32' is the
-- value of @GetLastError@.
data LinkError
  = LibraryNotFound String Word32
  | ProcNotFound String String Word32
  deriving stock (Eq, Show)

instance Exception LinkError

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "LoadLibraryExW"
  c_LoadLibraryExW :: CWString -> Ptr () -> Word32 -> IO (Ptr ())

foreign import ccall unsafe "GetProcAddress"
  c_GetProcAddress :: Ptr () -> CString -> IO (FunPtr a)

foreign import ccall unsafe "GetLastError"
  c_GetLastError :: IO Word32
#else
-- Elsewhere the library is only compiled and loaded (by tests, GHCi and
-- Template Haskell), so these stubs stand in for kernel32 and fail if
-- called.
c_LoadLibraryExW :: CWString -> Ptr () -> Word32 -> IO (Ptr ())
c_LoadLibraryExW _ _ _ = notWindows "LoadLibraryExW"

c_GetProcAddress :: Ptr () -> CString -> IO (FunPtr a)
c_GetProcAddress _ _ = notWindows "GetProcAddress"

c_GetLastError :: IO Word32
c_GetLastError = notWindows "GetLastError"

notWindows :: String -> IO a
notWindows f = ioError (userError (f ++ ": Windows functions can only be called on Windows"))
#endif

-- | Search the application directory, System32 and directories added with
-- @AddDllDirectory@, but not the current directory or @PATH@.
loadLibrarySearchDefaultDirs :: Word32
loadLibrarySearchDefaultDirs = 0x00001000

-- | Modules loaded so far. They are never freed.
libraries :: IORef (Map String (Ptr ()))
libraries = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE libraries #-}

loadLibrary :: String -> IO (Ptr ())
loadLibrary name = do
  cached <- Map.lookup name <$> readIORef libraries
  case cached of
    Just h -> pure h
    Nothing -> do
      h <- withCWString name $ \s -> c_LoadLibraryExW s nullPtr loadLibrarySearchDefaultDirs
      if h == nullPtr
        then c_GetLastError >>= throwIO . LibraryNotFound name
        else atomicModifyIORef' libraries (\m -> (Map.insert name h m, h))

-- | Find an exported function. Throws 'LinkError' if the DLL or the export
-- is missing.
resolveProc :: String -> String -> IO (FunPtr a)
resolveProc dll name = do
  h <- loadLibrary dll
  p <- withCString name (c_GetProcAddress h)
  if p == nullFunPtr
    then c_GetLastError >>= throwIO . ProcNotFound dll name
    else pure p
