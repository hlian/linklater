module Types(Bytes, Text, JPEGMonad, ExceptT(..)) where

import Control.Monad.Except (ExceptT(..))
import Data.ByteString (ByteString)
import Data.Text (Text)

type JPEGMonad = ExceptT String IO
type Bytes = ByteString
