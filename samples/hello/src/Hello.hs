{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Calls a few Windows functions through the generated bindings.
module Hello (hello) where

import Control.Exception (try)
import Control.Monad (when)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.OsString (OsString, decodeUtf, osstr)
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
  buffers
  errors

  when gui $ do
    -- String parameters take an OsString (or an OsPath); the caption is
    -- optional, so it is a Maybe.
    answer <- messageBoxW (HWND nullPtr) [osstr|Hello from Haskell!|] (Just [osstr|windows-hs|]) MB_OKCANCEL
    putStrLn (if answer == IDOK then "You pressed OK." else "You pressed Cancel.")

-- | Use constants that are strings, GUIDs and pointers given as integers.
constants :: IO ()
constants = do
  -- String constants point to NUL-terminated strings, and can be passed
  -- where a function takes a string.
  wide <- lstrlenW WC_BUTTONW
  ansi <- lstrlenA DD_BEEP_DEVICE_NAME
  putStrLn ("lstrlenW WC_BUTTONW: " ++ show wide ++ ", lstrlenA DD_BEEP_DEVICE_NAME: " ++ show ansi)
  -- lstrlenW counts UTF-16 units: the clef, outside the BMP, takes two.
  literal <- lstrlenW [osstr|héllo 𝄞|]
  putStrLn ("lstrlenW of 7 characters ending in U+1D11E: " ++ show literal)

  -- IDC_ARROW is the integer 32512 passed where a string is expected.
  HCURSOR cursor <- loadCursorW (HINSTANCE nullPtr) IDC_ARROW
  putStrLn ("Arrow cursor loaded: " ++ show (cursor /= nullPtr))

  -- A GUID constant, passed by reference. The wrapper returns the path,
  -- the function's last parameter, as an OsString, frees the copy Windows
  -- allocated, and throws if the HRESULT is a failure.
  documents <- with FOLDERID_Documents $ \folder -> shGetKnownFolderPath folder 0 (HANDLE nullPtr)
  putStrLn . ("Documents folder: " ++) =<< decodeUtf documents

  -- Constants are patterns too.
  let describe = \case
        HKEY_LOCAL_MACHINE -> "HKEY_LOCAL_MACHINE"
        _ -> "another key"
  putStrLn ("HKEY_LOCAL_MACHINE matches itself: " ++ describe HKEY_LOCAL_MACHINE)

-- | Strings that Windows writes to a buffer: the wrapper takes the size of
-- the buffer, allocates it, and returns the string with the C result.
buffers :: IO ()
buffers = do
  -- GetCurrentDirectoryW returns the size it needs, including the NUL, when
  -- the buffer is too small. Ask with an empty one first.
  (needed, _) <- getCurrentDirectoryW 0
  (written, directory) <- getCurrentDirectoryW needed
  putStrLn . (("Current directory (" ++ show written ++ " characters): ") ++) =<< decodeUtf directory

  -- GetModuleFileNameW cuts the name short to fit, and returns the size of
  -- the buffer when it does.
  (_, exe) <- getModuleFileNameW (HMODULE nullPtr) MAX_PATH
  (cut, start) <- getModuleFileNameW (HMODULE nullPtr) 4
  putStrLn . ("Program: " ++) =<< decodeUtf exe
  putStrLn . (("Its first 3 characters (" ++ show cut ++ "): ") ++) =<< decodeUtf (start :: OsString)

-- | Errors reported through @GetLastError@.
errors :: IO ()
errors = do
  -- CloseHandle returns BOOL and sets the last error, so its wrapper throws
  -- when it fails.
  closed <- try (closeHandle (HANDLE nullPtr))
  putStrLn ("CloseHandle of a null handle: " ++ either (\e -> show (e :: WindowsError)) (const "closed") closed)

  -- GetFileAttributesW returns a number, which the caller checks.
  attributes <- getFileAttributesW [osstr|C:\no\such\file|]
  if attributes == INVALID_FILE_ATTRIBUTES
    then lastError >>= \e -> putStrLn ("GetFileAttributesW failed: " ++ show e)
    else putStrLn "GetFileAttributesW found C:\\no\\such\\file"
