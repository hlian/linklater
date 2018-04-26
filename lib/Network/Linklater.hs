{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Network.Linklater
-- Copyright: (c) The Linklaterteers
-- License: BSD-style
-- Maintainer: hi@haolian.org
-- Stability: experimental
-- Portability: GHC
--
-- Here's a @/jpgto@ bot! If you run this program and then tell Slack
-- about your server (incoming hook and custom slash command) and then
-- type @/jpgto baby corgi@ in one of your channels, you'll get the
-- image from @http://baby.corgi.jpg.to@.
--
-- <https://github.com/hlian/linklater/blob/master/examples/JointPhotographicExpertsGroupTonga.hs>
--
-- One @/jpgto baby corgi@, et voila.
--
-- <<https://raw.githubusercontent.com/hlian/linklater/6232b950a333cfa6d5fffea997ec9ab8c2ce31ba/corgi.jpg>>

module Network.Linklater
       (
         -- * Types
         Channel(..),
         User(..),
         Message(..),
         Config(..),
         Command(..),
         Event(..),
         Icon(..),
         Format(..),
         -- * API calls
         say,
         startRTM,
         startRTMWithOptions,
         -- * HTTP bot servers
         slash,
         slashSimple,
         trigger
       ) where

import qualified Data.Aeson as Aeson
import           Network.HTTP.Types (status200, status400, parseSimpleQuery, ResponseHeaders)
import           Network.Wai (responseLBS, strictRequestBody, Application, Request)
import qualified Network.Wai as Wai
import qualified URI.ByteString as URI

import           Control.Lens
import           Control.Monad.Except
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.Aeson.Lens
import           Data.Text.Strict.Lens
import           Network.Linklater.Batteries
import           Network.Linklater.Types
import           Network.Wreq hiding (Response, params, headers)


headers :: ResponseHeaders
headers =
  [("Content-type", "text/plain")]

responseOf :: Status -> Text -> Wai.Response
responseOf status message =
  responseLBS status headers (message ^. (re utf8 . lazy))

-- | A bot server for people who are in a hurry. Make a function that
-- takes a 'Command' and returns some 'Text' in 'IO' world, and we'll
-- convert it into a 'Network.WAI' application. If you want more
-- control over the request and respond, see 'slash'.
slashSimple :: (Command -> IO Text) -> Application
slashSimple f =
  slash (\command _ respond -> f command >>= (respond . responseOf status200))

-- | A bot server! As if by magic. This acts like a 'Network.WAI'
-- middleware: Linklater wraps around your application. (Really, it
-- just gives you a 'Command' to work with instead of a raw HTTP
-- request.)
slash :: (Command -> Application) -> Application
slash inner req respond = do
  params <- _paramsIO req
  case commandOfParams params of
    Right command ->
      inner command req respond
    Left msg ->
      respond (responseOf status400 ("linklater: unable to parse request: " <> msg ^. packed))

trigger :: (Event -> Application) -> Application
eventTrigger inner req respond = do
  body <- strictRequestBody req
  case body ^? key "challenge" . _String of
    Just challenge -> respond $ responseOf status200 challenge
    Nothing ->
      case _eitherEvent body of
        Right event -> inner event req respond
        Left msg ->
          respond (responseOf status400 ("linklater: unable to parse request: " <> msg ^. packed))
----------------------------------------
-- ~ API calls ~

-- | I POST a 'Message' to Slack and return the HTTP response.
-- However, I need a 'Config' (containing an incoming hook configured
-- through Slack administration) first.
--
-- Guaranted to not throw an unchecked exception.
say :: (MonadError RequestError m, MonadIO m) => Message -> Config -> m ()
say message Config{..} = do
  _ <- tryRequest (postWith _reasonableOptions (_configHookURL ^. unpacked) (Aeson.encode message))
  pure ()

-- | I GET a WebSocket 'URI' from Slack's real-time messaging
-- endpoint. However, I need an 'APIToken' (configured through Slack
-- administration) first.
--
-- Guaranted to not throw an unchecked exception.
startRTM :: (MonadError RequestError m, MonadIO m) => APIToken -> m URI.URI
startRTM token =
  startRTMWithOptions (_reasonableOptions & authenticate)
  where
    authenticate =
       (param "token" .~ [view coerced token]) . (param "simple_latest" .~ ["1"]) . (param "no_unreads" .~ ["1"])

startRTMWithOptions :: (MonadError RequestError m, MonadIO m) => Options -> m URI.URI
startRTMWithOptions opts = do
  response <- tryRequest (getWith opts (_u "/api/rtm.start"))
  (value :: Aeson.Value) <- Aeson.eitherDecode (response ^. responseBody) & promoteEither response id
  rawURI <- value ^? key "url" . _String . re utf8 & promoteMaybe response (show value)
  URI.parseURI URI.strictURIParserOptions rawURI & promoteEither response show

----------------------------------------
-- ~ Helpers ~

-- | Disables Wreq's default behavior of throwing exceptions, which
-- seems reckless
_reasonableOptions :: Options
_reasonableOptions =
  defaults & checkResponse .~ Nothing

_eitherEvent :: ByteString -> Either String Event
_eitherEvent body = do
  bodyObj <- Aeson.eitherDecode body :: Either String Aeson.Object
  eventOfBody $ bodyObj ^. ix "event" . _Object 

_paramsIO :: Request -> IO (Map Text Text)
_paramsIO req = do
  lazyBytes <- strictRequestBody req
  let query = lazyBytes ^.. (strict . to parseSimpleQuery . traverse . to (both %~ view utf8))
  return (fromList query)

_u :: String -> String
_u = ("https://slack.com" ++)
