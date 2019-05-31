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
import Data.List

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
  , line :: Integer
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
  displayString :: String,
  start :: Location
  } deriving (Show, Generic)
instance FromJSON QuickInfoBody

data Location = Location {
  line :: Integer,
  offset :: Integer
  } deriving (Show, Generic)
instance FromJSON Location

data TypeDefinitionBody = TypeDefinitionBody {
  file :: String,
  start :: Location
  } deriving (Show, Generic)
instance FromJSON TypeDefinitionBody

data Msg a = Msg {
  seq :: Integer
  , body :: a
  } deriving (Show, Generic)
instance (FromJSON a) => FromJSON (Msg a)

fromRawJSONToJSON :: ByteString -> Maybe (Msg MsgBody)
fromRawJSONToJSON = decodeStrict

handleQuickInfoResponse :: ByteString -> Maybe (Msg QuickInfoBody)
handleQuickInfoResponse = decodeStrict

handleTypeDefinitionResponse :: ByteString -> Maybe (Msg [TypeDefinitionBody])
handleTypeDefinitionResponse = decodeStrict

data Partial = Partial {
  command :: String
  } deriving (Show, Generic)
instance FromJSON Partial

data Response =
    RQuickInfo (Maybe (Msg QuickInfoBody))
  | RMessage (Maybe (Msg MsgBody))
  | RTypeDefinition (Maybe (Msg [TypeDefinitionBody]))
  deriving Show

decoderRing' :: String -> ByteString -> Response
decoderRing' "navtree-full" bs = RMessage $ fromRawJSONToJSON bs
decoderRing' "quickinfo" bs = RQuickInfo $ handleQuickInfoResponse bs
decoderRing' "typeDefinition" bs = RTypeDefinition $ handleTypeDefinitionResponse bs
decoderRing' "definition" bs = RTypeDefinition $ handleTypeDefinitionResponse bs

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

getSpanStart :: MsgChild -> (Integer, Integer)
getSpanStart MsgChild{spans=spans} =
  head $ map (\MsgSpan{line=line,start=start} -> (line, start)) spans

toQuickInfoCommand :: Show a => a -> Integer -> Integer -> [Char]
toQuickInfoCommand filePath line offset =
  "{ \"seq\": 1, \"type\": \"request\", \"command\": \"quickinfo\", \"arguments\": { \"file\": "
  <> (show filePath)
  <> ", \"line\": " <> show line <> ", \"offset\": " ++ show (offset + 1) ++ " } }\n"

toQuickInfoCommands filePath Msg{body=MsgBody{childItems=childItems}} =
  foldr (++) "" $ map (\x -> toQuickInfoCommand filePath $ getSpanStart x) childItems

toTypeDefinitionCommand :: Show a => a -> Integer -> Integer -> [Char]
toTypeDefinitionCommand filePath line offset =
  "{ \"seq\": 2, \"type\": \"request\", \"command\": \"definition\", \"arguments\": { \"file\": "
  <> (show filePath)
  <> ", \"line\": " <> show (line) <> ", \"offset\": " ++ show (offset) ++ " } }\n"

checkIsAlias :: Msg QuickInfoBody -> Bool
checkIsAlias Msg{body=QuickInfoBody{displayString=displayString}} =
  "(alias)" `isPrefixOf` displayString

getOffsetFromQuickInfo :: Msg QuickInfoBody -> (Integer, Integer)
getOffsetFromQuickInfo Msg{body=QuickInfoBody{start=Location{line=line,offset=offset}}} = (line, offset)

resultHandler :: FilePath -> TChan ByteString -> TChan ByteString -> IO ()
resultHandler fp inchan outchan = do
  newValue <- atomically $ readTChan inchan
  putStrLn . show $ newValue
  cmd <- pure $ (decodeStrict newValue :: Maybe Partial)
  case decoderRing newValue of
    Just msg -> case msg of
      RQuickInfo (Just m) -> do
        -- so I think here, if it's an alias, emit new typeDefinition commands
        case (checkIsAlias m) of
          True ->
            let (line, offset) = getOffsetFromQuickInfo m
                cmd = toTypeDefinitionCommand fp line offset
            in do
              putStrLn $ cmd
              atomically $ writeTChan outchan $ BC.pack cmd
          False -> pure ()

        -- putStrLn "Here in RQuickInfo"
        -- putStrLn . show $ m

      RMessage (Just m) -> do
        -- putStrLn "Here in RMessage"
        -- putStrLn . show $ msg
        let cmds = toQuickInfoCommands fp m
        atomically $ writeTChan outchan $ BC.pack cmds

      RTypeDefinition (Just m) -> do
        -- putStrLn "Here in RTypeDefinition"
        -- putStrLn . show $ msg
        let Msg{body=body} = m
            files = map (\TypeDefinitionBody{file=file} -> file) body
        -- putStrLn $ "Well well " <> (show files)

        if length files > 0 then do
          putStrLn . show $ files
          atomically $ writeTChan outchan $ BC.pack (navtreeCommand (head files))
        else
          pure ()

      _ -> do
        -- putStrLn "Unknown message what do"
        -- putStrLn . show $ msg
        pure ()
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
