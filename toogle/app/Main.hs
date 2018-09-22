{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Process
import System.IO
import System.Directory
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import qualified Control.Concurrent.MVar as MV
import Data.Attoparsec.ByteString as Atto

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
import           System.IO.Streams (Generator, InputStream, OutputStream)
import qualified System.IO.Streams as Streams

import Lib

openCommand filePath =
  "{ \"seq\": 0, \"type\": \"request\", \"command\": \"open\", \"arguments\": { \"file\": "
  ++ (show filePath)
  ++ " } }\n"

navtreeCommand filePath =
  "{ \"seq\": 1, \"type\": \"request\", \"command\": \"navtree-full\", \"arguments\": { \"file\": "
  ++ (show filePath)
  ++ " } }\n"

infoCommand filePath =
  "{ \"seq\": 2, \"type\": \"request\", \"command\": \"quickinfo\", \"arguments\": { \"file\": "
  ++ (show filePath)
  ++ ", \"line\": 1, \"offset\": 63 } }\n"

writeConsole :: IO (OutputStream ByteString)
writeConsole = Streams.makeOutputStream $ \m -> case m of
  Just bs ->
    BC.putStrLn $ (BC.append "this is a line: " bs)
  Nothing -> pure ()

--
-- Parsing
--
-- raw -> attoparsec -> aeson
--
-- TSServer returns two lines we don't need before hitting the raw JSON
-- (content-length & a newline)
-- so we need to dump those before we can parse the response with aeson

handleRaw = do
  -- now that's a lazy way to get to the JSON
  _ <- Atto.takeWhile $ Atto.notInClass "{"
  -- just consumes and returns the rest
  takeByteString


toJSONFromTS :: IO (OutputStream ByteString)
toJSONFromTS = Streams.makeOutputStream $ \m -> case m of
  Just raw -> do
    result <- pure $ Atto.parseOnly handleRaw raw
    BC.putStrLn . BC.pack $ show $ result
  Nothing -> pure ()

mkOutHandler :: Queue -> Handle -> IO ()
mkOutHandler (Queue mvar) hout = do
  inputStream <- Streams.handleToInputStream hout
-- outStream <- writeConsole
--  forkIO $
--    Streams.supply inputStream outStream
  toJSONStream <- toJSONFromTS
  forkIO $
    Streams.supply inputStream toJSONStream
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

data Queue = Queue (MV.MVar String)

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver    = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"
      exampleFile = curDir ++ "/testing.ts"

  (hin, hout, err) <- mkProcess tsserver


  cmdInput <- mkInHandler hin
  forkIO $ Streams.write (Just . BC.pack $ openCommand exampleFile) cmdInput

  dats <- MV.newMVar ""
  forkIO $ mkOutHandler (Queue dats) hout

  -- ok, so we have to wait until the telemetryEventName projectInfo
  -- looks like that only happens with larger projects, maybe we need to
  -- listen to the PID returned bytestring typingsInstallerPid to close?

  threadDelay(1000000)

  forkIO $ do
    Streams.write (Just . BC.pack $ navtreeCommand exampleFile) cmdInput
    Streams.write (Just . BC.pack $ infoCommand exampleFile) cmdInput

  -- fascinating.  the commands are one-based offset for line + offset
  -- the server's responses are zero based for lines, 1 based for offset
  -- it seems.

  -- outHandler needs to write to an mvar.

  threadDelay(1000000000)
