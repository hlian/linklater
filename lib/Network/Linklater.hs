{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
         say,
         slash,
         slashSimple,
         Channel(..),
         User(..),
         Message(..),
         Config(..),
         Command(..),
         Icon(..),
         Format(..)
       ) where

import           BasePrelude hiding ((&), lazy)
import           Control.Lens
import           Data.Text.Strict.Lens
import           Network.Linklater.Types

import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai

import           Data.Map (Map, fromList)
import           Data.Text (Text)
import           Network.HTTP.Types (status200, status400, parseSimpleQuery, ResponseHeaders)
import           Network.Wai (responseLBS, strictRequestBody, Application, Request)
import           Network.Wreq hiding (params, headers)

headers :: ResponseHeaders
headers =
  [("Content-type", "text/plain")]

responseOf :: Status -> Text -> Wai.Response
responseOf status message =
  responseLBS status headers (message ^. (re utf8 . lazy))

-- | The 'say' function posts a 'Message', with a capital M, to Slack.
-- It'll, however, need a 'Config' (a.k.a. incoming token) first.
say :: Message -> Config -> IO (Response Text)
say message Config{..} = do
  response <- post (_configHookURL ^. unpacked) (Aeson.encode message)
  return (response <&> (^. strict . utf8))

-- | A bot server for people who are in a hurry. Make a function that
-- takes a 'Command' and returns some 'Text' in 'IO' world, and we'll
-- convert it into a 'Network.WAI' application. If you want more
-- control over the request and respond, see 'slash'.
slashSimple :: (Command -> IO Text) -> Application
slashSimple f =
  slash (\command _ respond -> f command >>= (respond . responseOf status200))

paramsIO :: Request -> IO (Map Text Text)
paramsIO req = do
  lazyBytes <- strictRequestBody req
  let query = lazyBytes ^.. (strict . to parseSimpleQuery . traverse . to (both %~ view utf8))
  return (fromList query)

-- | A bot server! As if by magic. This acts like a 'Network.WAI'
-- middleware: Linklater wraps around your application. (Really, it
-- just gives you a 'Command' to work with instead of a raw HTTP
-- request.)
slash :: (Command -> Application) -> Application
slash inner req respond = do
  params <- paramsIO req
  case commandOfParams params of
    Right command ->
      inner command req respond
    Left msg ->
      respond (responseOf status400 ("linklater: unable to parse request: " <> msg ^. packed))
