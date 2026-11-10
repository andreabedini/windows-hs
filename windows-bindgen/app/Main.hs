module Main (main) where

import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr)
import WinMD (readDatabase)
import Windows.Bindgen

data Options = Options
  { optModule :: Text
  , optOutput :: Maybe FilePath
  , optNamesFile :: [FilePath]
  , optLayoutChecks :: Maybe FilePath
  , optSys :: Bool
  , optHelp :: Bool
  }

defaults :: Options
defaults = Options "Windows.Win32" Nothing [] Nothing False False

options :: [OptDescr (Options -> Options)]
options =
  [ Option "m" ["module"] (ReqArg (\m o -> o {optModule = T.pack m}) "NAME") "name of the generated module (default: Windows.Win32)"
  , Option "o" ["output"] (ReqArg (\f o -> o {optOutput = Just f}) "FILE") "write the module to FILE instead of standard output"
  , Option "n" ["names"] (ReqArg (\f o -> o {optNamesFile = optNamesFile o ++ [f]}) "FILE") "read names from FILE, one per line; # starts a comment"
  , Option "" ["c-layout-checks"] (ReqArg (\f o -> o {optLayoutChecks = Just f}) "FILE") "also write C static assertions for struct layouts to FILE"
  , Option "" ["sys"] (NoArg (\o -> o {optSys = True})) "keep every function's C signature (no error checks or returned out parameters), like windows-sys"
  , Option "h" ["help"] (NoArg (\o -> o {optHelp = True})) "show this help"
  ]

usage :: String -> String
usage prog =
  usageInfo
    ( unlines
        [ "Generate Haskell bindings from Windows metadata."
        , ""
        , "Usage: " ++ prog ++ " [OPTION]... WINMD [NAME]..."
        , ""
        , "Each NAME is a namespace, such as Windows.Win32.Foundation, or a"
        , "namespace and a type, function or constant, such as"
        , "Windows.Win32.UI.WindowsAndMessaging.MessageBoxW. The module contains"
        , "the named items and every type they depend on."
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
  (winmd, cliNames) <- case positional of
    w : ns -> pure (w, map T.pack ns)
    [] -> do
      hPutStrLn stderr (prog ++ ": missing WINMD argument\nTry '" ++ prog ++ " --help'.")
      exitFailure
  fileNames <- concat <$> mapM readNames (optNamesFile opts)
  let names = fileNames ++ cliNames
  when (null names) $ do
    hPutStrLn stderr (prog ++ ": no names given; nothing to generate")
    exitFailure
  db <-
    readDatabase winmd >>= \case
      Right db -> pure db
      Left e -> hPutStrLn stderr (prog ++ ": " ++ winmd ++ ": " ++ show e) >> exitFailure
  let cfg = defaultConfig {cfgModuleName = optModule opts, cfgSys = optSys opts}
  case bindgen cfg db names of
    Left es -> do
      mapM_ (hPutStrLn stderr . ((prog ++ ": ") ++)) es
      exitFailure
    Right out -> do
      mapM_ (hPutStrLn stderr . ((prog ++ ": warning: skipped ") ++)) (outWarnings out)
      maybe (T.putStr (outModule out)) (\f -> T.writeFile f (outModule out)) (optOutput opts)
      mapM_ (\f -> T.writeFile f (cLayoutChecks out)) (optLayoutChecks opts)

readNames :: FilePath -> IO [Text]
readNames f = parseNames <$> T.readFile f
