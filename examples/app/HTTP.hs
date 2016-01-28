{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HTTP where

import BasePrelude hiding ((&), putStrLn, lazy)
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Data.Aeson
import Network.Wreq

import Types

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
