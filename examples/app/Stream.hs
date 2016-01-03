{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

import           BasePrelude hiding ((&), putStrLn)
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Text.IO
import           Data.Text.Strict.Lens
import           Network.Wreq
import           URI.ByteString

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Network.WebSockets as Sock
import qualified System.Environment as Env
import qualified Wuss as Sock

type Bytes = ByteString
data Brain = Brain { _api, _bearer :: !Text } deriving (Show)
data World = World { _wss :: !URI } deriving (Show)
data Speech = Speech { _channel, _t :: !Text } deriving (Show)
data Speech' = Speech' { _speech :: !Speech, _id :: !Int } deriving (Show)

makeLenses ''Brain
makeLenses ''World

u :: String -> String
u = ("https://slack.com" ++)

instance FromJSON World where
  parseJSON (Object o) = do
    ok <- o .: "ok"
    url <- o .: "url"
    guard ok
    either (fail . show) (return . World) (parseURI strictURIParserOptions (url ^. re utf8))

  parseJSON invalid =
    typeMismatch "World" invalid

instance ToJSON Speech' where
  toJSON (Speech' (Speech channel t) id_) =
    object ["id" .= id_, "channel" .= channel, "text" .= t, "type" .= ("message" :: String)]

hum :: (MonadReader Brain m, MonadIO m) => m World
hum = do
  api_ <- view api
  let opts = defaults & param "token" .~ [api_]
                      & param "simple_latest" .~ ["1"]
                      & param "no_unreads" .~ ["1"]
  resp <- liftIO (getWith opts (u "/api/rtm.start"))
  case eitherDecode (resp ^. responseBody) of
    Right world -> do
      liftIO (print $ resp ^? responseBody . key "url")
      return world
    Left err ->
      error err

stage0 :: IO World
stage0 = do
  Just apiToken <- Env.lookupEnv "API_TOKEN"
  Just bearerToken <- Env.lookupEnv "BEARER_TOKEN"
  let brain = Brain (apiToken ^. packed) (bearerToken ^. packed)
  runReaderT hum brain

stage1 :: URI -> Chan Speech -> IO (Chan Bytes)
stage1 uri outbox =
  case (uri ^? uriAuthorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked,
        uri ^? uriPathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      chan <- newChan
      Sock.runSecureClient host 443 path (consumer chan)
      return chan
    _ ->
      error ("invalid url: " <> show uri)
  where
    consumer chan conn = do
      void $ forkIO (forever worker)
      void $ forkIO (forever listener)
      where
        worker = do
          msg <- Sock.receiveData conn
          writeChan chan msg
        listener = do
          speech <- readChan outbox
          Sock.sendTextData conn (encode (Speech' speech 1))

withInbox :: Chan Bytes -> (Bytes -> IO a) -> IO ()
withInbox inbox cont = do
  chan <- dupChan inbox
  (void . forkIO . forever) $ do
    bytes <- readChan chan
    cont bytes

logChan :: Chan Bytes -> IO ()
logChan inbox =
  withInbox inbox $ \msg ->
    putStrLn (msg ^. utf8)

parseChan :: Chan Bytes -> Chan Speech -> IO ()
parseChan inbox outbox =
  withInbox inbox $ \msg ->
    case (msg ^? key "text" . _String, msg ^? key "channel" . _String, msg ^? key "reply_to") of
      (Just speech, Just channel, Nothing) ->
        writeChan outbox (Speech channel speech)
      _ ->
        return ()

-- | Empties out the original channel, so as to prevent memory leaks.
sinkChan :: Chan Bytes -> IO ()
sinkChan originalChan =
  (void . forkIO . forever) $ readChan originalChan

main :: IO ()
main = void $ do
  outbox <- newChan
  world <- stage0
  chan <- stage1 (world ^. wss) outbox
  logChan chan
  parseChan chan outbox
  sinkChan chan
