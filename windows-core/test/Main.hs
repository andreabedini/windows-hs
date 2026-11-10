-- | Tests for the parts of the runtime that do not call Windows.
module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Windows.Core.HRESULT

main :: IO ()
main = do
  let failures =
        concat
          [ -- Values from winerror.h: E_ACCESSDENIED is
            -- HRESULT_FROM_WIN32(ERROR_ACCESS_DENIED).
            hresultFromWin32 5 `shouldBe` HRESULT (-2147024891)
          , hresultFromWin32 2 `shouldBe` HRESULT (-2147024894)
          , hresultFromWin32 0 `shouldBe` HRESULT 0
          , -- A value that is already an HRESULT (E_POINTER) is unchanged.
            hresultFromWin32 0x80004003 `shouldBe` HRESULT (-2147467261)
          , map succeeded [HRESULT 0, HRESULT 1, HRESULT (-2147467261)] `shouldBe` [True, True, False]
          , show (WindowsError (HRESULT (-2147024894)) "The system cannot find the file specified.")
              `shouldBe` "WindowsError 0x80070002: The system cannot find the file specified."
          , show (WindowsError (HRESULT (-2147467261)) "") `shouldBe` "WindowsError 0x80004003"
          ]
  mapM_ putStrLn failures
  unless (null failures) exitFailure
  putStrLn "ok"

shouldBe :: (Eq a, Show a) => a -> a -> [String]
shouldBe actual expected
  | actual == expected = []
  | otherwise = ["expected " ++ show expected ++ ", got " ++ show actual]
