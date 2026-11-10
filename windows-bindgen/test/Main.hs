-- | Tests for the generator. They read Windows.Win32.winmd (see
-- scripts/fetch-metadata.sh; WINMD_PATH overrides the location) and the
-- sample in samples/hello.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.Char (isAsciiLower)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
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

  -- CONTEXT has x86, x64 and ARM64 definitions, each with its own nested
  -- unions; only the x64 one may be chosen. In C it is DECLSPEC_ALIGN(16),
  -- which win32metadata does not record (see declaredAlign).
  test "per-architecture types with nested types" $
    pure $ case bindgen defaultConfig db ["Windows.Win32.System.Diagnostics.Debug.CONTEXT"] of
      Left es -> es
      Right out ->
        outWarnings out `shouldBe` []
          ++ [defLayout t | (t, "CONTEXT") <- outStructs out] `shouldBe` [Right (Layout 1232 16)]

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

  -- The rules of windows-rs 0.58, the last release generated from
  -- win32metadata.
  test "function wrappers check errors as windows-rs does" $ do
    let fns =
          [ "Windows.Win32.Foundation.CloseHandle" -- BOOL, sets the last error
          , "Windows.Win32.Storage.FileSystem.CreateFileW" -- HANDLE, sets the last error
          , "Windows.Win32.UI.Shell.SHGetKnownFolderPath" -- HRESULT and an out PWSTR
          , "Windows.Win32.System.Com.CoResumeClassObjects" -- HRESULT
          , "Windows.Win32.System.Com.CoCreateGuid" -- HRESULT and an out GUID
          , "Windows.Win32.System.Com.CoInitializeEx" -- HRESULT, but S_FALSE is a success too: kept
          , "Windows.Win32.System.SystemInformation.GetSystemTimeAsFileTime" -- void and an out FILETIME
          , "Windows.Win32.System.Registry.RegCloseKey" -- WIN32_ERROR, kept
          , "Windows.Win32.Storage.FileSystem.GetFileAttributesW" -- a number, kept
          , "Windows.Win32.Foundation.LocalFree" -- a handle, but excluded
          ]
        signatures cfg = signaturesOf db cfg fns
    pure $
      signatures defaultConfig
        `shouldBe` Right
          [ "closeHandle :: HANDLE -> IO ()"
          , "coCreateGuid :: IO GUID"
          , "coInitializeEx :: Ptr () -> Word32 -> IO HRESULT"
          , "coResumeClassObjects :: IO ()"
          , "createFileW :: Core.WideString s1 => s1 -> Word32 -> FILE_SHARE_MODE -> Ptr SECURITY_ATTRIBUTES -> FILE_CREATION_DISPOSITION -> FILE_FLAGS_AND_ATTRIBUTES -> HANDLE -> IO HANDLE"
          , "getFileAttributesW :: Core.WideString s1 => s1 -> IO Word32"
          , "getSystemTimeAsFileTime :: IO FILETIME"
          , "localFree :: HLOCAL -> IO HLOCAL"
          , "regCloseKey :: HKEY -> IO WIN32_ERROR"
          , "shGetKnownFolderPath :: Ptr GUID -> Word32 -> HANDLE -> IO PWSTR"
          ]
        ++ ( (filter (`elem` ["closeHandle :: HANDLE -> IO BOOL", "getSystemTimeAsFileTime :: Ptr FILETIME -> IO ()"]) <$> signatures defaultConfig {cfgSys = True})
               `shouldBe` Right ["closeHandle :: HANDLE -> IO BOOL", "getSystemTimeAsFileTime :: Ptr FILETIME -> IO ()"]
           )

  -- Read-only, NUL-terminated wide strings take any Core.WideString
  -- (OsString, PWSTR, Maybe OsString); counted and ANSI strings stay raw.
  test "string parameters take a WideString" $ do
    let fns =
          [ "Windows.Win32.UI.WindowsAndMessaging.MessageBoxW" -- two optional strings
          , "Windows.Win32.Globalization.GetStringTypeW" -- counted by another parameter
          , "Windows.Win32.Storage.FileSystem.CreateFileA" -- ANSI
          ]
    pure $
      signaturesOf db defaultConfig fns
        `shouldBe` Right
          [ "createFileA :: PSTR -> Word32 -> FILE_SHARE_MODE -> Ptr SECURITY_ATTRIBUTES -> FILE_CREATION_DISPOSITION -> FILE_FLAGS_AND_ATTRIBUTES -> HANDLE -> IO HANDLE"
          , "getStringTypeW :: Word32 -> PWSTR -> Int32 -> Ptr Word16 -> IO ()"
          , "messageBoxW :: (Core.WideString s1, Core.WideString s2) => HWND -> s1 -> s2 -> MESSAGEBOX_STYLE -> IO MESSAGEBOX_RESULT"
          ]
        ++ ( (filter ("messageBoxW" `Text.isPrefixOf`) <$> signaturesOf db defaultConfig {cfgSys = True} fns)
               `shouldBe` Right ["messageBoxW :: HWND -> PWSTR -> PWSTR -> MESSAGEBOX_STYLE -> IO MESSAGEBOX_RESULT"]
           )

  -- Compiling this output takes about a minute; scripts/check-win32-surface.sh
  -- does that and checks its layouts.
  test "the Win32 package's functions generate without skips" $ do
    names <- parseNames <$> T.readFile "../coverage/win32/names.txt"
    pure $ case bindgen defaultConfig db names of
      Left es -> es
      Right out -> outWarnings out `shouldBe` []

  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " test(s) failed")
    exitFailure

-- | The type signatures of the generated functions.
signaturesOf :: Database -> Config -> [Text.Text] -> Either [String] [Text.Text]
signaturesOf db cfg fns = case bindgen cfg db fns of
  Left es -> Left es
  Right out -> Right [l | l <- Text.lines (outModule out), Just (c, _) <- [Text.uncons l], isAsciiLower c, not ("pattern " `Text.isPrefixOf` l), " :: " `Text.isInfixOf` l, not ("'" `Text.isInfixOf` l)]

generateSample :: Database -> IO (Either [String] Output)
generateSample db = do
  names <- parseNames <$> T.readFile (sampleDir </> "bindgen-names.txt")
  pure (bindgen defaultConfig db names)

shouldBe :: (Eq a, Show a) => a -> a -> [String]
shouldBe actual expected
  | actual == expected = []
  | otherwise = ["expected " ++ show expected ++ ", got " ++ show actual]
