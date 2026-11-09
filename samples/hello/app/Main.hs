module Main (main) where

import Hello (hello)
import System.Environment (getArgs)

-- | @hello@ shows a message box at the end; @hello --no-gui@ does not.
main :: IO ()
main = do
  args <- getArgs
  hello ("--no-gui" `notElem` args)
