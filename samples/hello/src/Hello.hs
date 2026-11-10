{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Calls a few Windows functions through the generated bindings.
module Hello (hello) where

import Control.Exception (try)
import Control.Monad (when)
import Foreign.C.String (peekCWString, withCWString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek)
import Windows.Core (WindowsError, lastError)
import Windows.Win32

-- | Print some system information. With 'True', also show a message box.
hello :: Bool -> IO ()
hello gui = do
  ticks <- getTickCount
  putStrLn ("Milliseconds since boot: " ++ show ticks)

  info <- alloca $ \p -> getSystemInfo p >> peek p
  putStrLn ("Page size: " ++ show info.dwPageSize)
  putStrLn ("Processors: " ++ show info.dwNumberOfProcessors)
  -- A member of the union in SYSTEM_INFO, read like a field.
  putStrLn ("Processor architecture: " ++ show info.anonymous.anonymous.wProcessorArchitecture)

  window <- windowFromPoint (POINT {x = 0, y = 0})
  putStrLn ("Window at (0, 0): " ++ show window)

  constants
  errors

  when gui $ do
    answer <-
      withCWString "Hello from Haskell!" $ \text ->
        withCWString "windows-hs" $ \caption ->
          messageBoxW (HWND nullPtr) (PWSTR (castPtr text)) (PWSTR (castPtr caption)) MB_OKCANCEL
    putStrLn (if answer == IDOK then "You pressed OK." else "You pressed Cancel.")

-- | Use constants that are strings, GUIDs and pointers given as integers.
constants :: IO ()
constants = do
  -- String constants point to NUL-terminated strings.
  wide <- lstrlenW WC_BUTTONW
  ansi <- lstrlenA DD_BEEP_DEVICE_NAME
  putStrLn ("lstrlenW WC_BUTTONW: " ++ show wide ++ ", lstrlenA DD_BEEP_DEVICE_NAME: " ++ show ansi)

  -- IDC_ARROW is the integer 32512 passed where a string is expected.
  HCURSOR cursor <- loadCursorW (HINSTANCE nullPtr) IDC_ARROW
  putStrLn ("Arrow cursor loaded: " ++ show (cursor /= nullPtr))

  -- A GUID constant, passed by reference. The wrapper returns the path,
  -- the function's last parameter, and throws if the HRESULT is a failure.
  PWSTR path <- with FOLDERID_Documents $ \folder -> shGetKnownFolderPath folder 0 (HANDLE nullPtr)
  documents <- peekCWString (castPtr path)
  coTaskMemFree (castPtr path)
  putStrLn ("Documents folder: " ++ documents)

  -- Constants are patterns too.
  let describe = \case
        HKEY_LOCAL_MACHINE -> "HKEY_LOCAL_MACHINE"
        _ -> "another key"
  putStrLn ("HKEY_LOCAL_MACHINE matches itself: " ++ describe HKEY_LOCAL_MACHINE)

-- | Errors reported through @GetLastError@.
errors :: IO ()
errors = do
  -- CloseHandle returns BOOL and sets the last error, so its wrapper throws
  -- when it fails.
  closed <- try (closeHandle (HANDLE nullPtr))
  putStrLn ("CloseHandle of a null handle: " ++ either (\e -> show (e :: WindowsError)) (const "closed") closed)

  -- GetFileAttributesW returns a number, which the caller checks.
  attributes <- withCWString "C:\\no\\such\\file" (getFileAttributesW . PWSTR . castPtr)
  if attributes == INVALID_FILE_ATTRIBUTES
    then lastError >>= \e -> putStrLn ("GetFileAttributesW failed: " ++ show e)
    else putStrLn "GetFileAttributesW found C:\\no\\such\\file"
