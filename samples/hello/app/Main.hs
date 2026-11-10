module Main (main) where

import Hello (hello)
import System.Environment (getArgs)

-- | @hello@ also shows a file dialog and a message box; @hello --no-gui@
-- does not.
main :: IO ()
main = do
  args <- getArgs
  hello ("--no-gui" `notElem` args)
