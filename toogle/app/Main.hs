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
import           Data.Aeson

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as BC

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

--
-- Parsing
--
-- raw -> aeson
--
-- TSServer returns two lines we don't need before hitting the raw JSON
-- (content-length & a newline)
-- so we need to dump those before we can parse the response with aeson

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

data QuickInfoBody = QuickInfoBody {
  displayString :: String
  } deriving (Show, Generic)
instance FromJSON QuickInfoBody

data Msg a = Msg {
  seq :: Integer
  , body :: a
  } deriving (Show, Generic)
instance (FromJSON a) => FromJSON (Msg a)

fromRawJSONToJSON :: ByteString -> Maybe (Msg MsgBody)
fromRawJSONToJSON = decodeStrict

handleQuickInfoResponse :: ByteString -> Maybe (Msg QuickInfoBody)
handleQuickInfoResponse = decodeStrict

data Partial = Partial {
  command :: String
  } deriving (Show, Generic)
instance FromJSON Partial

data Response =
  RQuickInfo (Maybe (Msg QuickInfoBody))
  | RMessage (Maybe (Msg MsgBody))
  deriving Show

decoderRing' :: String -> ByteString -> Response
decoderRing' "navtree-full" bs = RMessage $ fromRawJSONToJSON bs
decoderRing' "quickinfo" bs = RQuickInfo $ handleQuickInfoResponse bs

decoderRing :: ByteString -> Maybe Response
decoderRing msg = do
  cmd <- (decodeStrict msg :: Maybe Partial)
  return $ decoderRing' (command cmd) msg

--
-- From real JSON to commands for the server
--

data Argument = Argument {
  file :: String
  } deriving (Show, Generic)
instance ToJSON Argument

-- data Command = Command {
--   seq :: Integer
--   , _type :: String
--   , command :: String
--   , arguments :: Argument
--   } deriving (Show, Generic)
-- instance ToJSON Command

fromJust :: Maybe p -> p
fromJust Nothing  = error "Does not exist"
fromJust (Just t) = t

getSpanStart :: MsgChild -> Integer
getSpanStart MsgChild{spans=spans} =
  head $ map (\MsgSpan{start=start} -> start) spans

toQuickInfoCommand :: Show a => a -> Integer -> [Char]
toQuickInfoCommand filePath offset =
  "{ \"seq\": 2, \"type\": \"request\", \"command\": \"quickinfo\", \"arguments\": { \"file\": "
  <> (show filePath)
  <> ", \"line\": 1, \"offset\": " ++ show (offset + 1) ++ " } }\n"

toQuickInfoCommands filePath Msg{body=MsgBody{childItems=childItems}} =
  foldr (++) "" $ map (\x -> toQuickInfoCommand filePath $ getSpanStart x) childItems

resultHandler :: FilePath -> TChan ByteString -> TChan ByteString -> IO ()
resultHandler fp inchan outchan = do
  newValue <- atomically $ readTChan inchan
  putStrLn . show $ newValue
  cmd <- pure $ (decodeStrict newValue :: Maybe Partial)
  case decoderRing newValue of
    Just msg -> case msg of
      RQuickInfo (Just m) -> do
        putStrLn "Here in RQuickInfo"
        putStrLn . show $ m
      RMessage (Just m) -> do
        putStrLn "Here in RMessage"
        putStrLn . show $ msg
        let cmds = toQuickInfoCommands fp m
        atomically $ writeTChan outchan $ BC.pack cmds
      _ -> do
        putStrLn . show $ msg

    _ -> do
      pure ()
  resultHandler fp inchan outchan

mkProcess :: FilePath -> IO (Handle, Handle, Handle)
mkProcess tsserverLocation = do
  (hin, hout, err, pid) <-
    createProcess_ "tsserver" (proc tsserverLocation []){ std_in  = CreatePipe
                                                        , std_out = CreatePipe
                                                        , std_err = CreatePipe }
  return (fromJust hin, fromJust hout, fromJust err)

outputHandler :: Handle -> TChan ByteString -> IO ()
outputHandler hin chan = do
  newValue <- atomically $ readTChan chan

  hPutStr hin $ BC.unpack newValue
  outputHandler hin chan

inputHandler :: Handle -> TChan ByteString -> IO ()
inputHandler hout chan = do
  line <- hGetLine hout
  case length line of
    1 -> pure ()
    _ -> atomically $ do
      writeTChan chan (BC.pack $ line)
  inputHandler hout chan

main :: IO ()
main = do
  curDir <- makeAbsolute =<< getCurrentDirectory
  let tsserver    = curDir ++ "/tsserver/node_modules/typescript/bin/tsserver"
      exampleFile' = curDir ++ "/testing.ts"
      -- exampleFile = "/Users/iandavidson/jupiter/app/www/src/index.ts"
      exampleFile = "/Users/iandavidson/jupiter/packages/@ecomm/cart/panel/Panel.ts"

  (hin, hout, err) <- mkProcess tsserver

  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering

  fromOutputChan <- atomically $ newTChan
  forInputChan   <- atomically $ newTChan

  forkIO $ outputHandler hin forInputChan
  -- send out initial commands
  atomically $ writeTChan forInputChan $ BC.pack $ openCommand exampleFile
  atomically $ writeTChan forInputChan $ BC.pack $ navtreeCommand exampleFile

  forkIO $ inputHandler hout fromOutputChan
  forkIO $ resultHandler exampleFile fromOutputChan forInputChan

  threadDelay(1000000000)
