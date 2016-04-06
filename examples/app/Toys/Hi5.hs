{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Toys.Hi5 where

import BasePrelude hiding ((&), putStrLn, lazy)
import Control.Lens hiding ((.=), re)
import Data.Aeson
import Data.Aeson.Types
import Text.Printf.TH
import Text.Regex.PCRE.Heavy

import Types
import Utils

data Target = TargetOne { _victim :: !Text } | TargetAny deriving (Eq, Ord, Show)
data Want = Want { _target :: !Target, _line :: !Line } deriving (Eq, Ord, Show)
data Machine = Machine { _lines :: ![Want] } deriving (Show)
makeLenses ''Want

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

hi5 :: Chan Bytes -> Chan Speech -> IO ()
hi5 inbox outbox = do
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
