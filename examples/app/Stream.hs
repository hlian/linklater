{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import BasePrelude hiding ((&), putStrLn, lazy)
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import Data.Text.IO
import Data.Text.Strict.Lens
import URI.ByteString

import Toys.Hi5 (hi5)
import HTTP
import Live
import Types

import qualified Data.Text as Text
import qualified Network.WebSockets as Sock
import qualified System.Environment as Env
import qualified Wuss as Sock

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

-- | Empties out the original channel, so as to prevent memory leaks.
sinkChan :: Chan Bytes -> IO ()
sinkChan originalChan =
  (void . forever) $ readChan originalChan

jazzChan :: Chan Bytes -> Chan Speech -> IO ()
jazzChan inbox outbox = do
  countM <- newMVar (0 :: Int)
  withInbox inbox $ \line_ ->
    when (Text.isInfixOf ":raised_hands:" (line_ ^. truth))
         (do let victory = writeChan outbox (Speech line_  "JAZZ HANDS")
             let update = (`mod` 3) . (+ 1) &&& id
             count <- modifyMVar countM (return . update)
             when (count == 2) victory)

logChan :: Chan Bytes -> IO ()
logChan inbox =
  withInbox inbox putStrLn

main :: IO ()
main = void $ do
  outbox <- newChan
  world <- stage0
  inbox <- stage1 (world ^. wss) outbox
  hi5 inbox outbox
  jazzChan inbox outbox
  when debug (logChan inbox)
  sinkChan inbox
  where
    debug = False
