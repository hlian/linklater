{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           BasePrelude hiding ((&), putStrLn, lazy)
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text.IO
import           Data.Text.Strict.Lens
import           Text.Printf.TH
import           Network.Wreq
import           URI.ByteString

import           StateMachine
import           Types
import           Utils

import qualified Data.Text as Text
import qualified Network.WebSockets as Sock
import qualified System.Environment as Env
import qualified Wuss as Sock

data Brain = Brain { _api, _bearer :: !Text } deriving (Show)
data World = World { _wss :: !URI } deriving (Show)
data Speech = Speech { _replyTo :: !Line, _t :: !Text } deriving (Show)
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
  toJSON (Speech' (Speech line t) id_) =
    object [ "id" .= id_
           , "channel" .= (line ^. channel)
           , "text" .= t
           , "type" .= ("message" :: String)
           ]
  -- toEncoding (Speech' (Speech channel t) id_) =
  --   pairs ("id" .= id_ <> "channel" .= channel <> "text" .= t <> "type" .= ("message" :: String))

hum :: (MonadReader Brain m, MonadIO m) => m World
hum = do
  api_ <- view api
  let opts = defaults & param "token" .~ [api_]
                      & param "simple_latest" .~ ["1"]
                      & param "no_unreads" .~ ["1"]
  resp <- liftIO (getWith opts (u "/api/rtm.start"))
  case eitherDecode (resp ^. responseBody) of
    Right world ->
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

withInbox :: FromJSON a => Chan Bytes -> (a -> IO b) -> IO ()
withInbox inbox cont = do
  chan <- dupChan inbox
  (void . forkIO . forever) $ do
    bytes <- readChan chan
    case eitherDecode (bytes ^. lazy) of
      Left _ ->
        return ()
      Right o ->
        void (cont o)

logChan :: Chan Bytes -> IO ()
logChan inbox =
  withInbox inbox putStrLn

newMachine :: IO (MVar Machine)
newMachine = newMVar (Machine [])

updateMachine :: MVar Machine -> Want -> IO (Maybe Want)
updateMachine machineM want =
  modifyMVar machineM (return . feed want)

alert :: Want -> Want -> MVar Int -> IO Text
alert w0 w1 nonsenseIndexM =
  if u0 == u1 then do
    idx <- modifyMVar nonsenseIndexM (\idx -> return (idx + 1, idx `mod` length sadness))
    return $ [st|<@%s> high-fives <@%s>! %s.|] u0 u1 (sadness !! idx)
  else do
    idx <- modifyMVar nonsenseIndexM (\idx -> return (idx + 1, idx `mod` length nonsense))
    return $ [st|<@%s> and <@%s> high five! Their high-five %s.|] u0 u1 (nonsense !! idx)
  where
    u0 = w0 ^. line . user
    u1 = w1 ^. line . user

    nonsense :: [Text]
    nonsense = unsafePerformIO . shuffle $
               [ "sounds surprisingly wet"
               , "wakes up the owls next door"
               , "brings a tear to your eye"
               , "rouses the long-dormant autombile industry"
               , "can heardly be heard over the roar of the stadium"
               , "diminishes the aurora borealis (Northern Lights) happening behind it by comparison"
               , "reminds you of mitochondria, which are the powerhouses of the cell"
               , "erupts, at first in moans and then merely the sound of quivering flesh and sinew against bedsheets"
               , "is transcribed and filed away, central to a murder years later"
               ]

    sadness :: [Text]
    sadness = unsafePerformIO . shuffle $
              [ "People avert their eyes in shame"
              , "A howl of the lone wolf rises and falls"
              , "A murder of crows take air"
              , "The haunting sound of wind chimes plays a tune on this abandoned baby’s toes"
              , "The nasty pond ripples, then lies fallow"
              ]

parseChan :: Chan Bytes -> Chan Speech -> IO ()
parseChan inbox outbox = do
  machineM <- newMachine
  nonsenseIndexM <- newMVar 0
  withInbox inbox $ \want -> do
    maybeMatch <- updateMachine machineM want
    case maybeMatch of
      Just match -> do
        a <- alert match want nonsenseIndexM
        writeChan outbox (Speech (want ^. line) a)
      Nothing ->
        return ()

jazzChan :: Chan Bytes -> Chan Speech -> IO ()
jazzChan inbox outbox = do
  countM <- newMVar (0 :: Int)
  withInbox inbox $ \line_ ->
    when (Text.isInfixOf ":raised_hands:" (line_ ^. truth))
         (do let victory = writeChan outbox (Speech line_  "JAZZ HANDS")
             let update = (`mod` 3) . (+ 1) &&& id
             count <- modifyMVar countM (return . update)
             when (count == 2) victory)

-- | Empties out the original channel, so as to prevent memory leaks.
sinkChan :: Chan Bytes -> IO ()
sinkChan originalChan =
  (void . forever) $ readChan originalChan

main :: IO ()
main = void $ do
  outbox <- newChan
  world <- stage0
  chan <- stage1 (world ^. wss) outbox
  parseChan chan outbox
  jazzChan chan outbox
  sinkChan chan
