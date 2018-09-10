{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process
import System.IO
import System.Directory
import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           System.IO.Streams (Generator, InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Lib

exampleFile =
  "/home/ian/code/jupiter/packages/@ecomm/checkbox/Checkbox.tsx"

command =
  "{ \"seq\": 0, \"type\": \"request\", \"command\": \"open\", \"arguments\": { \"file\": "
  ++ (show exampleFile)
  ++ " } }"

writeConsole :: IO (OutputStream ByteString)
writeConsole = Streams.makeOutputStream $ \m -> case m of
  Just bs -> S.putStrLn bs
  Nothing -> pure ()

mkOutHandler :: Handle -> IO (OutputStream ByteString)
mkOutHandler hout = do
  inputStream <- Streams.handleToInputStream hout
  outputStream <- writeConsole
  Streams.connect inputStream outputStream
  pure outputStream

mkInHandler :: Handle -> IO (OutputStream ByteString)
mkInHandler hin = do
  s <- Streams.handleToOutputStream hin
  pure s

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"

  (hin, hout, err, pid) <-
    runInteractiveProcess tsserver [] Nothing Nothing

  mkOutHandler hout
  cmdInput <- mkInHandler hin

  Streams.write (Just command) cmdInput

  hPutStrLn hin command
  hPutStrLn hin command

  print $ tsserver

  pure ()
