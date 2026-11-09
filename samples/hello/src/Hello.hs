{-# LANGUAGE OverloadedRecordDot #-}

-- | Calls a few Windows functions through the generated bindings.
module Hello (hello) where

import Control.Monad (when)
import Foreign.C.String (withCWString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peek)
import Windows.Win32

-- | Print some system information. With 'True', also show a message box.
hello :: Bool -> IO ()
hello gui = do
  ticks <- getTickCount
  putStrLn ("Milliseconds since boot: " ++ show ticks)

  info <- alloca $ \p -> getSystemInfo p >> peek p
  putStrLn ("Page size: " ++ show info.dwPageSize)
  putStrLn ("Processors: " ++ show info.dwNumberOfProcessors)

  window <- windowFromPoint (POINT {x = 0, y = 0})
  putStrLn ("Window at (0, 0): " ++ show window)

  when gui $ do
    answer <-
      withCWString "Hello from Haskell!" $ \text ->
        withCWString "windows-hs" $ \caption ->
          messageBoxW (HWND nullPtr) (PWSTR (castPtr text)) (PWSTR (castPtr caption)) MB_OKCANCEL
    putStrLn (if answer == IDOK then "You pressed OK." else "You pressed Cancel.")
