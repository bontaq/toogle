module Main where

import System.Process
import System.IO
import System.Directory
import Control.Monad

import Lib

typeScriptLocation =
  "/home/ian/code/jupiter/node_modules/typescript/bin/tsserver"

exampleFile =
  "/home/ian/code/jupiter/packages/@ecomm/checkbox/Checkbox.tsx"

command =
  "{ \"seq\": 0, \"type\": \"request\", \"command\": \"open\", \"arguments\": { \"file\": " ++ (show exampleFile) ++ " } }"

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"

  (hin, hout, err, pid) <-
    runInteractiveProcess tsserver [] Nothing Nothing

  hPutStrLn hin command

  forever $ do
    hGetLine hout >>= print

  print $ tsserver

  pure ()
