module Main where

import System.Process

import Lib

typeScriptLocation =
  "/home/ian/code/jupiter/node_modules/typescript/bin/tsserver"

main :: IO ()
main = do
  (a, hout, b, c) <-
    createProcess (proc "ls" []){ std_out = CreatePipe }

  print $ hout
  print $ a
  print $ b
  -- print $ c

  pure ()
