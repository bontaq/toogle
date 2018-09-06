module Main where

import System.Process
import System.IO

import Lib

typeScriptLocation =
  "/home/ian/code/jupiter/node_modules/typescript/bin/tsserver"

main :: IO ()
main = do
  (_, hout, _, _) <-
    createProcess (proc "ls" []){ std_out = CreatePipe }

  case hout of
    Just (stuff) -> do
      hGetLine stuff >>= print

  print $ hout

  pure ()
