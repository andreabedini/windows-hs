-- | Tests for the generator. They read Windows.Win32.winmd and, from the
-- same directory, Microsoft.Direct3D.D3D12.winmd (see
-- scripts/fetch-metadata.sh; WINMD_PATH overrides the location of
-- Windows.Win32.winmd), and the sample in samples/hello.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.Char (isAsciiLower)
import Data.IORef
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.IO qualified as T
import System.Directory (findExecutable, getTemporaryDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
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
  let load p =
        readDatabase p >>= \case
          Right db -> pure db
          Left e -> do
            putStrLn ("cannot load " ++ p ++ ": " ++ show e)
            putStrLn "run scripts/fetch-metadata.sh or set WINMD_PATH"
            exitFailure
  db <- load path
  d3d12 <- load (takeDirectory path </> "Microsoft.Direct3D.D3D12.winmd")
  let dbs = [db, d3d12]
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
    pure $ case bindgen defaultConfig dbs ["Windows.Win32.System.Diagnostics.Debug.CONTEXT"] of
      Left es -> es
      Right out ->
        outWarnings out `shouldBe` []
          ++ [defLayout t | (t, "CONTEXT") <- outStructs out] `shouldBe` [Right (Layout 1232 16)]

  test "unknown names are errors" $
    pure $ case bindgen defaultConfig dbs ["Windows.Win32.Foundation.NoSuchThing"] of
      Left [_] -> []
      Left es -> ["expected one error, got " ++ show es]
      Right _ -> ["expected an error"]

  -- DML_BUFFER_BINDING, in Windows.Win32.winmd, holds an ID3D12Resource,
  -- which only Microsoft.Direct3D.D3D12.winmd defines.
  test "types from another metadata file" $ do
    let name = "Windows.Win32.AI.MachineLearning.DirectML.DML_BUFFER_BINDING"
    pure $ case (bindgen defaultConfig [db] [name], bindgen defaultConfig dbs [name]) of
      (Right alone, Right both) ->
        length (outWarnings alone) `shouldBe` 1
          ++ outWarnings both `shouldBe` []
          ++ [defLayout t | (t, "DML_BUFFER_BINDING") <- outStructs both] `shouldBe` [Right (Layout 24 8)]
          ++ check ("ID3D12Resource" `Text.isInfixOf` outModule both) "ID3D12Resource is not emitted"
      (alone, both) -> either id (const []) alone ++ either id (const []) both

  sample <- generateSample dbs
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

  -- The rules of windows-rs 0.58 (see 'functionShape').
  test "function wrappers check errors as windows-rs does" $ do
    let fns =
          [ "Windows.Win32.Foundation.CloseHandle" -- BOOL, sets the last error
          , "Windows.Win32.Storage.FileSystem.CreateFileW" -- HANDLE, sets the last error
          , "Windows.Win32.UI.Shell.SHGetKnownFolderPath" -- HRESULT and an out string
          , "Windows.Win32.System.Com.CoResumeClassObjects" -- HRESULT
          , "Windows.Win32.System.Com.CoCreateGuid" -- HRESULT and an out GUID
          , "Windows.Win32.System.Com.CoInitializeEx" -- HRESULT, but S_FALSE is a success too: kept
          , "Windows.Win32.System.SystemInformation.GetSystemTimeAsFileTime" -- void and an out FILETIME
          , "Windows.Win32.System.Registry.RegCloseKey" -- WIN32_ERROR, kept
          , "Windows.Win32.Storage.FileSystem.GetFileAttributesW" -- a number, kept
          , "Windows.Win32.Foundation.LocalFree" -- a handle, but excluded
          ]
        signatures cfg = signaturesOf dbs cfg fns
    pure $
      signatures defaultConfig
        `shouldBe` Right
          [ "closeHandle :: HANDLE -> IO ()"
          , "coCreateGuid :: IO GUID"
          , "coInitializeEx :: Ptr () -> Word32 -> IO HRESULT"
          , "coResumeClassObjects :: IO ()"
          , -- Named by SHGetKnownFolderPath's FreeWith.
            "coTaskMemFree :: Ptr () -> IO ()"
          , "createFileW :: Core.WideString s1 => s1 -> Word32 -> FILE_SHARE_MODE -> Ptr SECURITY_ATTRIBUTES -> FILE_CREATION_DISPOSITION -> FILE_FLAGS_AND_ATTRIBUTES -> HANDLE -> IO HANDLE"
          , "getFileAttributesW :: Core.WideString s1 => s1 -> IO Word32"
          , "getSystemTimeAsFileTime :: IO FILETIME"
          , "localFree :: HLOCAL -> IO HLOCAL"
          , "regCloseKey :: HKEY -> IO WIN32_ERROR"
          , "shGetKnownFolderPath :: Ptr GUID -> Word32 -> HANDLE -> IO Core.OsString"
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
      signaturesOf dbs defaultConfig fns
        `shouldBe` Right
          [ "createFileA :: PSTR -> Word32 -> FILE_SHARE_MODE -> Ptr SECURITY_ATTRIBUTES -> FILE_CREATION_DISPOSITION -> FILE_FLAGS_AND_ATTRIBUTES -> HANDLE -> IO HANDLE"
          , "getStringTypeW :: Word32 -> PWSTR -> Int32 -> Ptr Word16 -> IO ()"
          , "messageBoxW :: (Core.WideString s1, Core.WideString s2) => HWND -> s1 -> s2 -> MESSAGEBOX_STYLE -> IO MESSAGEBOX_RESULT"
          ]
        ++ ( (filter ("messageBoxW" `Text.isPrefixOf`) <$> signaturesOf dbs defaultConfig {cfgSys = True} fns)
               `shouldBe` Right ["messageBoxW :: HWND -> PWSTR -> PWSTR -> MESSAGEBOX_STYLE -> IO MESSAGEBOX_RESULT"]
           )

  -- Strings written to a buffer sized by an integer parameter, and
  -- returned strings freed with their FreeWith function, are OsStrings.
  test "returned strings are OsStrings" $ do
    let fns =
          [ "Windows.Win32.System.Environment.GetCurrentDirectoryW" -- a buffer
          , "Windows.Win32.Storage.FileSystem.GetVolumeInformationW" -- two buffers, BOOL checked
          , "Windows.Win32.Storage.FileSystem.GetFullPathNameW" -- and a pointer into it
          , "Windows.Win32.System.WindowsProgramming.GetComputerNameW" -- sized by pointer: raw
          , "Windows.Win32.Security.Credentials.CredMarshalCredentialW" -- FreeWith, but BOOL: raw
          ]
    pure $
      signaturesOf dbs defaultConfig fns
        `shouldBe` Right
          [ "credFree :: Ptr () -> IO ()"
          , "credMarshalCredentialW :: CRED_MARSHAL_TYPE -> Ptr () -> Ptr PWSTR -> IO ()"
          , "getComputerNameW :: PWSTR -> Ptr Word32 -> IO ()"
          , "getCurrentDirectoryW :: Word32 -> IO (Word32, Core.OsString)"
          , "getFullPathNameW :: Core.WideString s1 => s1 -> Word32 -> Ptr PWSTR -> IO (Word32, Core.OsString)"
          , "getVolumeInformationW :: Core.WideString s1 => s1 -> Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Word32 -> IO (Core.OsString, Core.OsString)"
          ]
        ++ ( (filter ("getCurrentDirectoryW" `Text.isPrefixOf`) <$> signaturesOf dbs defaultConfig {cfgSys = True} fns)
               `shouldBe` Right ["getCurrentDirectoryW :: Word32 -> PWSTR -> IO Word32"]
           )

  -- Vtable slots count the methods of every ancestor: IUnknown has 3,
  -- IModalWindow 1 (Show), so IFileDialog's own methods start at 4.
  -- ID3D12DescriptorHeap derives from ID3D12Pageable (0 methods),
  -- ID3D12DeviceChild (1) and ID3D12Object (4), so its own start at 8.
  -- GetCPUDescriptorHandleForHeapStart returns an 8-byte struct, which a C
  -- function returns in RAX but a method returns through a hidden pointer
  -- after this (the MSVC member-function ABI).
  --
  -- A method's FreeWith brings its free function along, as a function's
  -- does (IBackgroundCopyJob::GetDisplayName, CoTaskMemFree), and a type
  -- application picks the interface a Query method returns, so t comes first
  -- (IShellItem::BindToHandler).
  test "COM interfaces and methods" $ do
    let names =
          [ "Windows.Win32.System.Com.CoCreateInstance"
          , "Windows.Win32.UI.Shell.FileOpenDialog"
          , "Windows.Win32.UI.Shell.IFileOpenDialog"
          , "Windows.Win32.Graphics.Direct3D12.ID3D12DescriptorHeap"
          , "Windows.Win32.Networking.BackgroundIntelligentTransferService.IBackgroundCopyJob"
          ]
    pure $ case bindgen defaultConfig dbs names of
      Left es -> es
      Right out ->
        let ls = Text.lines (outModule out)
            slots =
              [ (Text.takeWhile (/= ' ') l, Text.takeWhile (/= ' ') rest)
              | (l, next) <- zip ls (drop 1 ls)
              , " (Core.upcast @" `Text.isInfixOf` l
              , Just rest <- [Text.stripPrefix "  Core.vtableEntry this " next]
              ]
            pick ns = sort (filter (\(n, _) -> n `elem` ns) slots)
            sigs ns = sort [l | l <- ls, any (\n -> (n <> " :: ") `Text.isPrefixOf` l) ns]
         in outWarnings out `shouldBe` []
              ++ pick ["iModalWindow_Show", "iFileDialog_SetFileTypes", "iFileDialog_SetTitle", "iFileDialog_GetResult", "iFileOpenDialog_GetResults", "iFileOpenDialog_GetSelectedItems"]
                `shouldBe` sort
                  [ ("iModalWindow_Show", "3")
                  , ("iFileDialog_SetFileTypes", "4")
                  , ("iFileDialog_SetTitle", "17")
                  , ("iFileDialog_GetResult", "20")
                  , ("iFileOpenDialog_GetResults", "27")
                  , ("iFileOpenDialog_GetSelectedItems", "28")
                  ]
              ++ pick ["id3D12DescriptorHeap_GetDesc", "id3D12DescriptorHeap_GetCPUDescriptorHandleForHeapStart"]
                `shouldBe` sort [("id3D12DescriptorHeap_GetDesc", "8"), ("id3D12DescriptorHeap_GetCPUDescriptorHandleForHeapStart", "9")]
              ++ sigs ["coCreateInstance", "iFileDialog_SetTitle", "iFileDialog_GetResult", "id3D12DescriptorHeap_GetCPUDescriptorHandleForHeapStart", "iShellItem_BindToHandler", "iBackgroundCopyJob_GetDisplayName"]
                `shouldBe` sort
                  [ "coCreateInstance :: Core.Interface t => Ptr GUID -> P.Maybe IUnknown -> CLSCTX -> IO t"
                  , "id3D12DescriptorHeap_GetCPUDescriptorHandleForHeapStart :: Core.Inherits ID3D12DescriptorHeap this => this -> IO D3D12_CPU_DESCRIPTOR_HANDLE"
                  , "iFileDialog_GetResult :: Core.Inherits IFileDialog this => this -> IO IShellItem"
                  , "iFileDialog_SetTitle :: (Core.Inherits IFileDialog this, Core.WideString s1) => this -> s1 -> IO ()"
                  , "iShellItem_BindToHandler :: (Core.Interface t, Core.Inherits IShellItem this, Core.Inherits IBindCtx i1) => this -> i1 -> Ptr GUID -> IO t"
                  , "iBackgroundCopyJob_GetDisplayName :: Core.Inherits IBackgroundCopyJob this => this -> IO Core.OsString"
                  ]
              ++ [l | l <- ls, "instance Core.Inherits " `Text.isPrefixOf` l, " IFileOpenDialog" `Text.isSuffixOf` l]
                `shouldBe` [ "instance Core.Inherits IFileOpenDialog IFileOpenDialog"
                           , "instance Core.Inherits IFileDialog IFileOpenDialog"
                           , "instance Core.Inherits IModalWindow IFileOpenDialog"
                           , "instance Core.Inherits IUnknown IFileOpenDialog"
                           ]
              ++ check ("pattern FileOpenDialog :: GUID" `elem` ls) "FileOpenDialog is not a GUID pattern"
              ++ check
                (any ("Core.returnStruct (\\r -> call'id3D12DescriptorHeap_GetCPUDescriptorHandleForHeapStart" `Text.isInfixOf`) ls)
                "an 8-byte struct result of a method is not returned through a hidden pointer"

  -- Compiling this output takes about a minute; scripts/check-win32-surface.sh
  -- does that and checks its layouts.
  test "the Win32 package's functions generate without skips" $ do
    names <- parseNames <$> T.readFile "../coverage/win32/names.txt"
    pure $ case bindgen defaultConfig dbs names of
      Left es -> es
      Right out -> outWarnings out `shouldBe` []

  n <- readIORef failures
  when (n > 0) $ do
    putStrLn (show n ++ " test(s) failed")
    exitFailure

-- | The type signatures of the generated functions.
signaturesOf :: [Database] -> Config -> [Text.Text] -> Either [String] [Text.Text]
signaturesOf dbs cfg fns = case bindgen cfg dbs fns of
  Left es -> Left es
  Right out -> Right [l | l <- Text.lines (outModule out), Just (c, _) <- [Text.uncons l], isAsciiLower c, not ("pattern " `Text.isPrefixOf` l), " :: " `Text.isInfixOf` l, not ("'" `Text.isInfixOf` l)]

generateSample :: [Database] -> IO (Either [String] Output)
generateSample dbs = do
  names <- parseNames <$> T.readFile (sampleDir </> "bindgen-names.txt")
  pure (bindgen defaultConfig dbs names)

shouldBe :: (Eq a, Show a) => a -> a -> [String]
shouldBe actual expected
  | actual == expected = []
  | otherwise = ["expected " ++ show expected ++ ", got " ++ show actual]

check :: Bool -> String -> [String]
check ok msg = if ok then [] else [msg]
