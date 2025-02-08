module Main where

import Control.Monad
import System.Environment
import Experiments

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ error "args: file.csv True/False <n>"
  let (file:_) = args
  let checkAll = read (args !! 1)
  let iters = read (args !! 2)
  all_experiments' file checkAll iters
