{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module StateMachine(
    feed
  , Machine(..)
  , Line(..)
  , Want(..)
--- Lenses
  , channel
  , line
  , user
  , truth
) where

import BasePrelude hiding ((&), lazy)
import Control.Lens hiding (re)
import Data.Aeson
import Data.Aeson.Types
import Text.Regex.PCRE.Heavy

import Types

data Target = TargetOne { _victim :: !Text } | TargetAny deriving (Eq, Ord, Show)
data Line = Line { _channel :: !Text, _user :: !Text, _truth :: !Text } deriving (Eq, Ord, Show)
data Want = Want { _target :: !Target, _line :: !Line } deriving (Eq, Ord, Show)
data Machine = Machine { _lines :: ![Want] } deriving (Show)
makeLenses ''Line
makeLenses ''Want

instance FromJSON Line where
  parseJSON (Object o) = do
    reply_to <- o .:? "reply_to"
    guard (not (isReply reply_to))
    Line <$> o .: "channel" <*> o .: "user" <*> o .: "text"
    where
      isReply :: Maybe Int -> Bool
      isReply = isJust

  parseJSON invalid =
    typeMismatch "Line" invalid

instance FromJSON Want where
  parseJSON whole@(Object o) = do
    line_ <- parseJSON whole
    Just target_ <- parseTarget <$> o .: "text"
    return (Want target_ line_)

  parseJSON invalid =
    typeMismatch "Want" invalid

parseTarget' :: Text -> [(Text, [Text])]
parseTarget' = scan [re|:hand:\s*(?:<@(.+?)>|)|]

parseTarget :: Text -> Maybe Target
parseTarget = listToMaybe . map (uncurry f) . parseTarget'
  where
    f :: Text -> [Text] -> Target
    f _ (username:_)=
      TargetOne username
    f _ [] =
      TargetAny

matches :: Want -> Want -> Bool
matches want0 want1 =
  (want0 ^. line . channel == want1 ^. line . channel) && matchingWant
  where
    matchingWant =
      case (want0 ^. target, want1 ^. target) of
        (TargetAny, TargetAny) ->
          True
        (TargetAny, TargetOne t) ->
          t == want0 ^. line . user
        (TargetOne t, TargetAny) ->
          t == want1 ^. line . user
        (TargetOne t, TargetOne t') ->
          t == want1 ^. line . user && t' == want0 ^. line .user

feed :: Want -> Machine -> (Machine, Maybe Want)
feed want0 (Machine wants) =
  (_1 %~ (Machine . nub)) $ case listToMaybe good of
    Just match ->
      (tail good <> bad, Just match)
    Nothing ->
      (want0 : wants, Nothing)
  where
    (good, bad) = partition (matches want0) wants
