{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module StateMachine(
    feed
  , Machine(..)
  , Line(..)
  , user
) where

import BasePrelude hiding ((&), lazy)
import Control.Lens hiding (re)
import Data.Aeson
import Data.Aeson.Types
import Text.Regex.PCRE.Heavy

import Types

data Want = WantOne { _target :: !Text } | WantAny deriving (Eq, Ord, Show)
data Line = Line { _want :: !Want, _channel :: !Text, _user :: !Text } deriving (Eq, Ord, Show)
data Machine = Machine { _lines :: ![Line] } deriving (Show)
makeLenses ''Line

instance FromJSON Line where
  parseJSON (Object o) = do
    reply_to <- o .:? "reply_to"
    Just want_ <- parseWant <$> o .: "text"
    guard (not (isReply reply_to))
    Line <$> pure want_ <*> o .: "channel" <*> o .: "user"
    where
      isReply :: Maybe Int -> Bool
      isReply = isJust

  parseJSON invalid =
    typeMismatch "Line" invalid

parseWant' :: Text -> [(Text, [Text])]
parseWant' = scan [re|:hand:\s*(?:<@(.+?)>|)|]

parseWant :: Text -> Maybe Want
parseWant = listToMaybe . map (uncurry f) . parseWant'
  where
    f :: Text -> [Text] -> Want
    f _ (username:_)=
      WantOne username
    f _ [] =
      WantAny

matches :: Line -> Line -> Bool
matches line0 line1 =
  (line0 ^. channel == line1 ^. channel) && matchingWant
  where
    matchingWant =
      case (line0 ^. want, line1 ^. want) of
        (WantAny, WantAny) ->
          True
        (WantAny, WantOne target) ->
          target == line0 ^. user
        (WantOne target, WantAny) ->
          target == line1 ^. user
        (WantOne target, WantOne target') ->
          target == line1 ^. user && target' == line0 ^. user

feed :: Line -> Machine -> (Machine, Maybe Line)
feed line0 (Machine lines_) =
  (_1 %~ (Machine . nub)) $ case listToMaybe good of
    Just match ->
      (tail good <> bad, Just match)
    Nothing ->
      (line0 : lines_, Nothing)
  where
    (good, bad) = partition (matches line0) lines_
