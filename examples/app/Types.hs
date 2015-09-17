module Types where
import Control.Monad.Except (ExceptT(..))
type JPEGMonad = ExceptT String IO

