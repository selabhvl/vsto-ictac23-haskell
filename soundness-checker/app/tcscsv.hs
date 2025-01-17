module Main where

import Control.Monad
import System.Environment
import Experiments

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ error "args: file.csv True/False"
  let file = head args
  let checkAll = read (args !! 1)
  all_experiments' file checkAll