module Main (main) where

import Control.Monad (forM, unless, when)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Console.GetOpt
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStr, hPutStrLn, stderr)
import WinMD (readDatabase)
import Windows.Bindgen

data Options = Options
  { optInputs :: [FilePath]
  , optModule :: Text
  , optOutput :: Maybe FilePath
  , optNamesFile :: [FilePath]
  , optLayoutChecks :: Maybe FilePath
  , optSys :: Bool
  , optHelp :: Bool
  }

defaults :: Options
defaults = Options [] "Windows.Win32" Nothing [] Nothing False False

options :: [OptDescr (Options -> Options)]
options =
  [ Option "i" ["in"] (ReqArg (\f o -> o {optInputs = optInputs o ++ [f]}) "PATH") "read metadata from PATH, a .winmd file or a directory of them; repeatable"
  , Option "m" ["module"] (ReqArg (\m o -> o {optModule = T.pack m}) "NAME") "name of the generated module (default: Windows.Win32)"
  , Option "o" ["output"] (ReqArg (\f o -> o {optOutput = Just f}) "FILE") "write the module to FILE instead of standard output"
  , Option "n" ["names"] (ReqArg (\f o -> o {optNamesFile = optNamesFile o ++ [f]}) "FILE") "read names from FILE, one per line; # starts a comment"
  , Option "" ["c-layout-checks"] (ReqArg (\f o -> o {optLayoutChecks = Just f}) "FILE") "also write C static assertions for struct layouts to FILE"
  , Option "" ["sys"] (NoArg (\o -> o {optSys = True})) "keep every function's C signature (no error checks, OsString strings, returned out parameters or generic interface parameters), like windows-sys"
  , Option "h" ["help"] (NoArg (\o -> o {optHelp = True})) "show this help"
  ]

usage :: String -> String
usage prog =
  usageInfo
    ( unlines
        [ "Generate Haskell bindings from Windows metadata."
        , ""
        , "Usage: " ++ prog ++ " --in PATH [OPTION]... [NAME]..."
        , ""
        , "Each NAME is a namespace, such as Windows.Win32.Foundation, or a"
        , "namespace and a type, function or constant, such as"
        , "Windows.Win32.UI.WindowsAndMessaging.MessageBoxW. The module contains"
        , "the named items and every type they depend on."
        , ""
        , "Give every metadata file the names need: types in one file can come"
        , "from another, as Windows.Win32.winmd uses Direct3D 12 types from"
        , "Microsoft.Direct3D.D3D12.winmd. --in with the directory both are in"
        , "reads them all."
        , ""
        , "Example:"
        , "  " ++ prog ++ " --in metadata -o src/Windows/Win32.hs \\"
        , "    Windows.Win32.System.SystemInformation.GetTickCount"
        ]
    )
    options

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  let (fs, positional, errs) = getOpt Permute options args
      opts = foldl (flip id) defaults fs
  unless (null errs) $ do
    hPutStr stderr (concat errs ++ "Try '" ++ prog ++ " --help'.\n")
    exitFailure
  when (optHelp opts) $ putStr (usage prog) >> exitSuccess
  when (null (optInputs opts)) $ do
    hPutStrLn stderr (prog ++ ": no metadata given; pass --in FILE.winmd or --in DIRECTORY\nTry '" ++ prog ++ " --help'.")
    exitFailure
  fileNames <- concat <$> mapM readNames (optNamesFile opts)
  let names = fileNames ++ map T.pack positional
  when (null names) $ do
    hPutStrLn stderr (prog ++ ": no names given; nothing to generate")
    exitFailure
  winmds <- concat <$> mapM (expandInput prog) (optInputs opts)
  dbs <- forM winmds $ \winmd ->
    readDatabase winmd >>= \case
      Right db -> pure db
      Left e -> hPutStrLn stderr (prog ++ ": " ++ winmd ++ ": " ++ show e) >> exitFailure
  let cfg = defaultConfig {cfgModuleName = optModule opts, cfgSys = optSys opts}
  case bindgen cfg dbs names of
    Left es -> do
      mapM_ (hPutStrLn stderr . ((prog ++ ": ") ++)) es
      exitFailure
    Right out -> do
      mapM_ (hPutStrLn stderr . ((prog ++ ": warning: skipped ") ++)) (outWarnings out)
      maybe (T.putStr (outModule out)) (\f -> T.writeFile f (outModule out)) (optOutput opts)
      mapM_ (\f -> T.writeFile f (cLayoutChecks out)) (optLayoutChecks opts)

-- | The metadata files an --in argument names: the file itself, or the
-- .winmd files in a directory, in name order.
expandInput :: String -> FilePath -> IO [FilePath]
expandInput prog path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then pure [path]
    else do
      files <- sort . filter ((== ".winmd") . takeExtension) <$> listDirectory path
      when (null files) $ do
        hPutStrLn stderr (prog ++ ": " ++ path ++ ": no .winmd files in this directory")
        exitFailure
      pure (map (path </>) files)

readNames :: FilePath -> IO [Text]
readNames f = parseNames <$> T.readFile f
