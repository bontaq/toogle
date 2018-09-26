{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import GHC.Generics

import System.Process
import System.IO
import System.Directory
import Control.Monad
import Control.Monad.STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import qualified Control.Concurrent.MVar as MV
import Data.Attoparsec.ByteString as Atto
import           Data.Aeson
-- import           Data.Aeson.Lens        (key, _String)
import qualified Data.Aeson.Types       as DAT

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

toJSONFromTS :: TChan (Maybe Msg) -> IO (OutputStream ByteString)
toJSONFromTS chan = Streams.makeOutputStream $ \m -> case m of
  Just raw -> do
    result <- pure $ Atto.parseOnly handleRaw raw
    case result of
      Right json -> do
        ans <- pure $ fromRawJSONToJSON json
        putStrLn "complete"
        atomically $ writeTChan chan ans
        -- putStrLn "wait"
        pure ()
      Left  _    ->  putStrLn "parsing messed up"
  Nothing -> pure ()

--
-- From attoparsec to real JSON
--

data MsgSpan = MsgSpan {
  start :: Integer
  } deriving (Show, Generic)
instance FromJSON MsgSpan

data MsgChild = MsgChild {
  text :: String
  , kind :: String
  , spans :: [MsgSpan]
  } deriving (Show, Generic)
instance FromJSON MsgChild

data MsgBody = MsgBody {
  childItems :: [MsgChild]
  } deriving (Show, Generic)
instance FromJSON MsgBody

data Msg = Msg {
  seq :: Integer
  , body :: MsgBody
  } deriving (Show, Generic)
instance FromJSON Msg

fromRawJSONToJSON ::
  ByteString -> Maybe Msg
fromRawJSONToJSON a = decodeStrict a :: Maybe Msg

--
-- From real JSON to commands for the server
--

data Argument = Argument {
  file :: String
  } deriving (Show, Generic)
instance ToJSON Argument

data Command = Command {
  seq :: Integer
  , _type :: String
  , command :: String
  , arguments :: Argument
  } deriving (Show, Generic)
instance ToJSON Command

mkOutHandler :: TChan (Maybe Msg) -> Handle -> IO ()
mkOutHandler chan hout = do
  inputStream <- Streams.handleToInputStream hout
--  outStream <- writeConsole
--  forkIO $
--    Streams.supply inputStream outStream
  toJSONStream <- toJSONFromTS chan
  forkIO $
    Streams.connect inputStream toJSONStream
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

data Queue = Queue (MV.MVar [(Maybe Msg)])

getSpanStart :: MsgChild -> Integer
getSpanStart MsgChild{spans=spans} =
  head $ map (\MsgSpan{start=start} -> start) spans

toQuickInfoCommand :: Show a => a -> Integer -> [Char]
toQuickInfoCommand filePath offset =
  "{ \"seq\": 2, \"type\": \"request\", \"command\": \"quickinfo\", \"arguments\": { \"file\": "
  ++ (show filePath)
  ++ ", \"line\": 1, \"offset\": " ++ show (offset) ++ " } }\n"

toQuickInfoCommands filePath Msg{body=MsgBody{childItems=childItems}} =
  map (\x -> toQuickInfoCommand filePath $ getSpanStart x) childItems

resultHandler :: FilePath -> TChan (Maybe Msg) -> IO ()
resultHandler fp chan = do
  newValue <- atomically $ readTChan chan

  case newValue of
    Just msg -> do
      let cmds = toQuickInfoCommands fp msg
      putStrLn . show $ toQuickInfoCommands fp msg
--      mapM_ (\x -> forkIO $ do
--                Streams.write (Just . BC.pack $ x) cmdInput) cmds

      -- mapM_ $ forkIO $ do Streams.write (Just . BC.pack) $ msg
    Nothing  -> putStrLn "Nada"
  -- putStrLn $ "read new value: " ++ show newValue
  resultHandler fp chan

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver    = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"
      exampleFile = curDir ++ "/testing.ts"

  (hin, hout, err) <- mkProcess tsserver

  cmdInput <- mkInHandler hin
  Streams.write (Just . BC.pack $ openCommand exampleFile) cmdInput

  -- dats <- MV.newMVar []

  -- ok, so we have to wait until the telemetryEventName projectInfo
  -- looks like that only happens with larger projects, maybe we need to
  -- listen to the PID returned bytestring typingsInstallerPid to close?

  -- threadDelay(1000000)

  chan <- atomically $ newTChan
  -- newCmdInput <- mkInHandler hin
  forkIO $ resultHandler exampleFile chan
  forkIO $ mkOutHandler chan hout


--  let readLoop = loop
--        where
--          loop = do
--            s <- MV.takeMVar dats
--            case s of
--              [] -> do
--                MV.putMVar dats s
--                threadDelay(1000000)
--                loop
--              (m:rest) ->
--                case m of
--                  Nothing -> MV.putMVar dats rest
--                  Just msg -> do
--                    let commands = toQuickInfoCommands exampleFile msg
--                    -- putStrLn "wot"
--                    putStrLn $ "something" ++  show commands
--                    -- putStrLn $ cmdInput
--                    mapM_ (\x -> Streams.write (Just . BC.pack $ x) cmdInput) commands
--                    pure ()
--
--            threadDelay(1000000)
--            loop


  forkIO $ do
    Streams.write (Just . BC.pack $ navtreeCommand exampleFile) cmdInput
    Streams.write (Just . BC.pack $ infoCommand exampleFile) cmdInput
    Streams.write (Just . BC.pack $ navtreeCommand exampleFile) cmdInput

  -- forkIO $ do readLoop

  -- fascinating.  the commands are one-based offset for line + offset
  -- the server's responses are zero based for lines, 1 based for offset
  -- it seems.

  -- outHandler needs to write to an mvar.

  threadDelay(1000000000)
