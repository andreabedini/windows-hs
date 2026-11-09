-- | Tests for the generator. They read Windows.Win32.winmd (see
-- scripts/fetch-metadata.sh; WINMD_PATH overrides the location) and the
-- sample in samples/hello.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text.IO qualified as T
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)
import WinMD
import Windows.Bindgen
import Windows.Bindgen.Layout
import Windows.Bindgen.Names

sampleDir :: FilePath
sampleDir = "../samples/hello"

main :: IO ()
main = do
  path <- fromMaybe "../metadata/Windows.Win32.winmd" <$> lookupEnv "WINMD_PATH"
  db <-
    readDatabase path >>= \case
      Right db -> pure db
      Left e -> do
        putStrLn ("cannot load " ++ path ++ ": " ++ show e)
        putStrLn "run scripts/fetch-metadata.sh or set WINMD_PATH"
        exitFailure
  failures <- newIORef (0 :: Int)
  let test name body = do
        putStr (name ++ ": ")
        hFlush stdout
        r <- try (body >>= evaluate)
        case r of
          Right [] -> putStrLn "ok"
          Right errs -> do
            putStrLn "FAILED"
            mapM_ (putStrLn . ("  " ++)) errs
            modifyIORef' failures (+ 1)
          Left (e :: SomeException) -> do
            putStrLn ("FAILED with exception: " ++ show e)
            modifyIORef' failures (+ 1)

  test "value names" $
    pure $
      map valueName ["GetTickCount", "WSAStartup", "DPA_Create", "hWnd", "Anonymous", "RECT", "type", "_lclose", "3D"]
        `shouldBe` ["getTickCount", "wsaStartup", "dpa_Create", "hWnd", "anonymous", "rect", "type_", "_lclose", "v3D"]

  test "constructor names" $
    pure $
      map conName ["HWND", "tagRECT", "_Anonymous_e__Union", "3D"]
        `shouldBe` ["HWND", "TagRECT", "T_Anonymous_e__Union", "T3D"]

  test "dedupe" $
    pure (dedupe mempty ["a", "b", "a", "a_", "a"] `shouldBe` ["a", "b", "a_", "a__", "a___"])

  test "struct layouts" $ do
    let layout ns n = maybe (Left "missing") defLayout (findTypeDef db ns n)
    pure $
      concat
        [ layout "Windows.Win32.Foundation" "RECT" `shouldBe` Right (Layout 16 4)
        , layout "Windows.Win32.System.SystemInformation" "SYSTEM_INFO" `shouldBe` Right (Layout 48 8)
        , layout "Windows.Win32.System.SystemInformation" "OSVERSIONINFOW" `shouldBe` Right (Layout 276 4)
        , layout "Windows.Win32.UI.WindowsAndMessaging" "WNDCLASSW" `shouldBe` Right (Layout 72 8)
        , layout "Windows.Win32.Foundation" "HWND" `shouldBe` Right (Layout 8 8)
        ]

  test "unknown names are errors" $
    pure $ case bindgen defaultConfig db ["Windows.Win32.Foundation.NoSuchThing"] of
      Left [_] -> []
      Left es -> ["expected one error, got " ++ show es]
      Right _ -> ["expected an error"]

  sample <- generateSample db
  test "sample matches the committed module" $ do
    committed <- T.readFile (sampleDir </> "src/Windows/Win32.hs")
    pure $ case sample of
      Left es -> es
      Right out ->
        outWarnings out `shouldBe` []
          ++ if outModule out == committed
            then []
            else ["samples/hello/src/Windows/Win32.hs is out of date; regenerate it (see samples/hello/bindgen-names.txt)"]

  test "sample layouts match the Windows headers" $
    findExecutable "x86_64-w64-mingw32-gcc" >>= \case
      Nothing -> putStr "(skipped: x86_64-w64-mingw32-gcc not found) " >> pure []
      Just gcc -> case sample of
        Left es -> pure es
        Right out -> do
          tmp <- getTemporaryDirectory
          let file = tmp </> "windows-bindgen-layout-checks.c"
          T.writeFile file (cLayoutChecks out)
          (code, _, err) <- readProcessWithExitCode gcc ["-std=c11", "-fsyntax-only", file] ""
          pure (if code == ExitSuccess then [] else [err])

  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " test(s) failed")
    exitFailure

generateSample :: Database -> IO (Either [String] Output)
generateSample db = do
  names <- parseNames <$> T.readFile (sampleDir </> "bindgen-names.txt")
  pure (bindgen defaultConfig db names)

shouldBe :: (Eq a, Show a) => a -> a -> [String]
shouldBe actual expected
  | actual == expected = []
  | otherwise = ["expected " ++ show expected ++ ", got " ++ show actual]
