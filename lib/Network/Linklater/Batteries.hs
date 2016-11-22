{-# LANGUAGE NoImplicitPrelude #-}

module Network.Linklater.Batteries (
  module X
  , present
  ) where

import BasePrelude as X hiding ((&), lazy, putStrLn)
import Control.Monad.Catch as X
  (MonadThrow(..))
import Control.Monad.Reader as X
  (MonadIO(..))
import Data.Map as X (Map)
import Data.Text as X (Text)
import Data.Text.IO as X (putStrLn)
import GHC.Exts as X (fromList)

import Control.Lens
import Data.Text.Strict.Lens

present :: Show a => a -> Text
present = view packed . show
