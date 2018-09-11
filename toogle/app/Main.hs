{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process
import System.IO
import System.Directory
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
import           System.IO.Streams (Generator, InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Lib

exampleFile =
  "/Users/iandavidson/jupiter/packages/@ecomm/checkbox/Checkbox.tsx"

openCommand =
  "{ \"seq\": 0, \"type\": \"request\", \"command\": \"open\", \"arguments\": { \"file\": "
  ++ (show exampleFile)
  ++ " } }\n"

navtreeCommand =
  "{ \"seq\": 1, \"type\": \"request\", \"command\": \"navtree-full\", \"arguments\": { \"file\": "
  ++ (show exampleFile)
  ++ " } }\n"

infoCommand =
  "{ \"seq\": 1, \"type\": \"request\", \"command\": \"quickinfo\", \"arguments\": { \"file\": "
  ++ (show exampleFile)
  ++ ", \"line\": 0, \"offset\": 0 } }\n"

writeConsole :: IO (OutputStream ByteString)
writeConsole = Streams.makeOutputStream $ \m -> case m of
  Just bs -> S.putStrLn bs
  Nothing -> pure ()

mkOutHandler :: Handle -> IO ()
mkOutHandler hout = do
  inputStream <- Streams.handleToInputStream hout
  outputStream <- writeConsole
  Streams.connect inputStream outputStream
  pure ()

mkInHandler :: Handle -> IO (OutputStream ByteString)
mkInHandler hin = do
  s <- Streams.handleToOutputStream hin
  pure s

fromJust :: Maybe p -> p
fromJust Nothing  = error "Does not exist"
fromJust (Just t) = t

mkProcess :: FilePath -> IO (Handle, Handle, Handle)
mkProcess tsserverLocation = do
  (hin, hout, err, pid) <-
    createProcess_ "tsserver" (proc tsserverLocation []){ std_in  = CreatePipe
                                                        , std_out = CreatePipe
                                                        , std_err = CreatePipe }
  return (fromJust hin, fromJust hout, fromJust err)

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"

  (hin, hout, err) <- mkProcess tsserver

  cmdInput <- mkInHandler hin
  forkIO $ Streams.write (Just $ BC.pack openCommand) cmdInput

  forkIO $ mkOutHandler hout

  forkIO $ Streams.write (Just $ BC.pack navtreeCommand) cmdInput

  threadDelay(1000000000)
