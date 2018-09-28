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
import Data.Attoparsec.ByteString as Atto
import           Data.Aeson
-- import           Data.Aeson.Lens        (key, _String)
import qualified Data.Aeson.Types       as DAT

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC
import           System.IO.Streams (Generator, InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Concurrent as Concurrent

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
    BC.putStr $ (BC.append "" bs)
  Nothing -> pure ()

writeStream :: OutputStream ByteString -> IO (OutputStream ByteString)
writeStream os = Streams.makeOutputStream $ \m -> case m of
  Just bs -> do
    forkIO $ do
      Streams.write (Just . BC.pack . show $ bs) os
      BC.putStr $ (BC.append "" bs)
    pure ()
    -- pure ()
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

--toJSONFromTS :: IO (OutputStream ByteString)
--toJSONFromTS = Streams.makeOutputStream $ \m -> case m of
--  Just raw -> do
--    result <- pure $ Atto.parseOnly handleRaw raw
--    case result of
--      Right json -> do
--        ans <- pure $ fromRawJSONToJSON json
--        putStrLn "complete"
--        -- atomically $ writeTChan ans
--        -- putStrLn "wait"
--        pure ans
--      Left  _    ->  putStrLn "parsing messed up"
--  Nothing -> pure ()

parseToAtto :: InputStream ByteString -> IO (InputStream ByteString)
parseToAtto f = Streams.makeInputStream $ do
  m <- Streams.read f
  case m of
    Just raw -> do
      result <- pure $ Atto.parseOnly handleRaw raw
      case result of
        Right a -> return $ Just a
    Nothing  -> return $ Nothing

parseToMsg :: InputStream ByteString -> IO (InputStream (Maybe Msg))
parseToMsg f = Streams.makeInputStream $ do
  m <- Streams.read f
  case m of
    Just raw -> do
      let result = decodeStrict raw :: Maybe Msg
      pure $ Just result
    Nothing -> pure Nothing

parseToStr :: InputStream (Maybe Msg) -> IO (InputStream ByteString)
parseToStr f = Streams.makeInputStream $ do
  m <- Streams.read f
  case m of
    Just a -> pure $ Just $ BC.pack $ show a

parseToCommand :: FilePath -> InputStream (Maybe Msg) -> IO (InputStream ByteString)
parseToCommand fp f = Streams.makeInputStream $ do
  m <- Streams.read f
  case m of
    _ -> pure . Just . BC.pack . show $ navtreeCommand fp
    Just a -> case a of
      Just z -> do
        putStrLn . show $ z
        --pure $ Just . BC.pack . show $ toQuickInfoCommands fp z
        pure . Just . BC.pack . show $ navtreeCommand fp
      Nothing -> pure $ Just . BC.pack $ ""
    Nothing -> pure $ Just . BC.pack $ ""

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

mkOutHandler :: Handle -> IO (InputStream ByteString)
mkOutHandler hout = do
  inputStream <- Streams.handleToInputStream hout
  pure inputStream
--  outStream <- writeConsole
--  forkIO $
--    Streams.supply inputStream outStream
--  toJSONStream <- toJSONFromTS
--  forkIO $
--    Streams.connect inputStream toJSONStream

mkInHandler :: Handle -> IO (OutputStream ByteString)
mkInHandler hin =
  Streams.handleToOutputStream hin

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

getSpanStart :: MsgChild -> Integer
getSpanStart MsgChild{spans=spans} =
  head $ map (\MsgSpan{start=start} -> start) spans

toQuickInfoCommand :: Show a => a -> Integer -> [Char]
toQuickInfoCommand filePath offset =
  "{ \"seq\": 2, \"type\": \"request\", \"command\": \"quickinfo\", \"arguments\": { \"file\": "
  ++ (show filePath)
  ++ ", \"line\": 1, \"offset\": " ++ show (offset) ++ " } }\n"

toQuickInfoCommands filePath Msg{body=MsgBody{childItems=childItems}} =
  foldr (++) "" $ map (\x -> toQuickInfoCommand filePath $ getSpanStart x) childItems

resultHandler :: FilePath -> TChan String -> TChan String -> IO ()
resultHandler fp inchan outchan = do
  newValue <- atomically $ readTChan inchan
  putStrLn . show $ newValue
  atomically $ writeTChan outchan (navtreeCommand fp)
  resultHandler fp inchan outchan

outputHandler :: Handle -> TChan String -> IO ()
outputHandler hin chan = do
  newValue <- atomically $ readTChan chan
  -- putStrLn . show $ newValue
  hPutStrLn hin newValue
  outputHandler hin chan

inputHandler :: Handle -> TChan String -> IO ()
inputHandler hout chan = do
  line <- hGetLine hout
  case length line of
    1 -> pure ()
    _ -> atomically $ do
      writeTChan chan line
  inputHandler hout chan

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver    = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"
      exampleFile = curDir ++ "/testing.ts"

  (hin, hout, err) <- mkProcess tsserver

  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering

  fromOutputChan <- atomically $ newTChan
  forInputChan   <- atomically $ newTChan

  hPutStrLn hin (openCommand exampleFile)

  forkIO $
    do inputHandler hout fromOutputChan
  forkIO $
    do resultHandler exampleFile fromOutputChan forInputChan
  forkIO $
    do outputHandler hin forInputChan

  threadDelay(1000000000)
