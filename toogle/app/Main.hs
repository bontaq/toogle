module Main where

import System.Process
import System.IO

import Lib

typeScriptLocation =
  "/home/ian/code/jupiter/node_modules/typescript/bin/tsserver"

exampleFile =
  "/home/ian/code/jupiter/packages/@ecomm/checkbox/Checkbox.tsx"

command =
  "{ \"seq\": 0, \"type\": \"request\", \"command\": \"open\", \"arguments\": { \"file\": " ++ (show exampleFile) ++ " } }"

main :: IO ()
main = do
  (hin, hout, _, _) <-
    createProcess_ "wew" (proc typeScriptLocation []){ std_out = CreatePipe
                                                     , std_in  = CreatePipe }

  case hin of
    Just (stuff) -> do
      hPutStrLn stuff command

  case hout of
    Just (stuff) -> do
      hGetContents stuff >>= print

  print $ hout

  pure ()
