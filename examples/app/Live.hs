{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Live where

import BasePrelude hiding ((&), putStrLn, lazy)
import Control.Lens hiding ((.=))
import Data.Aeson

import Types

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
