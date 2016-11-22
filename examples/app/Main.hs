{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Network.Linklater.Batteries hiding (Chan)
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Data.Aeson
import Data.Text.Strict.Lens
import URI.ByteString
import System.IO hiding (utf8, putStrLn)

import Toys.Hi5 (hi5)
import Toys.SummonPainting (summon)
import Types

import qualified Network.Linklater as Linklater
import qualified Control.Exception.Safe as Ex
import qualified Network.WebSockets as Sock
import qualified System.Environment as Env
import qualified Wuss as Sock
import qualified Control.Concurrent.STM.TBMChan as Chan

type Chan = Chan.TBMChan

stage0 :: IO URI
stage0 = do
  Just (view packed -> apiToken) <- Env.lookupEnv "API_TOKEN"
  uriEither <- runExceptT (Linklater.startRTM (Linklater.APIToken apiToken))
  case uriEither of
    Left err ->
      error ("stage0: error while grabbing an RTM token: " <> show err)
    Right uri -> do
      putStrLn ("stage0: grabbed RTM token: " <> present uri)
      pure uri

stage1 :: URI -> Chan Speech -> IO (Chan Bytes)
stage1 uri outbox =
  case (uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked,
        uri ^? pathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      chan <- atomically $ Chan.newTBMChan 1
      forkIO $ Sock.runSecureClient host 443 path (withConnection chan)
      return chan
    _ ->
      error ("invalid url: " <> show uri)
  where
    withConnection chan conn =
      forkIO (forever writer) >> reader
      where
        writer = do
          msg <- Sock.receiveData conn
          putStrLn (fromMaybe "<>"  $ msg ^? utf8)
          atomically $ Chan.writeTBMChan chan msg
        reader = do
          speechMaybe <- atomically $ Chan.tryReadTBMChan outbox
          case speechMaybe of
            Just (Just speech) -> do
              Sock.sendTextData conn (encode (Speech' speech 1))
              reader
            Just Nothing ->
              reader
            Nothing ->
              pure ()

-- jazzChan :: Chan Bytes -> Chan Speech -> IO ()
-- jazzChan inbox outbox = do
--   countM <- newMVar (0 :: Int)
--   withInbox inbox $ \line_ ->
--     when (Text.isInfixOf ":raised_hands:" (line_ ^. truth))
--          (do let victory = writeChan outbox (Speech line_  "JAZZ HANDS")
--              let update = (`mod` 3) . (+ 1) &&& id
--              count <- modifyMVar countM (return . update)
--              when (count == 2) victory)

-- logChan :: Chan Bytes -> IO ()
-- logChan inbox =
--   withInbox inbox putStrLn

speech0 = Speech (Line "a" "b" "whatever") "hey"

main :: IO ()
main = lineBuffering $ do
  outbox <- atomically $ Chan.newTBMChan 1
  rtmToken <- stage0
  inbox <- stage1 rtmToken outbox
  forkIO $ forever $ threadDelay 2000000 >> atomically (Chan.writeTBMChan outbox speech0)
  forever $ do
    msg <- atomically $Chan.readTBMChan inbox
    pure ()
  where
    debug = False

lineBuffering :: IO () -> IO ()
lineBuffering action = do
  buffering <- hGetBuffering stdout
  bracket (hSetBuffering stdout LineBuffering) (const $ hSetBuffering stdout buffering) (const action)
