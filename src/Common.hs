module Common (
    module Common
  , module Control.Monad.IO.Class
  ) where

import Data.Text
import Data.String.Conversions
import Text.Printf
import Debug.Trace
import Control.Monad.IO.Class
import Text.Pretty.Simple

sqlWhereEquals :: String -> Text
sqlWhereEquals c = cs (printf "\"%s\"= ?" c :: String)

mydbg :: Show a => String -> a -> a
mydbg s = traceShow
  <$> ((++) s . show)
  <*> id
-- mydbg _ = id

myTrace :: String -> ()
myTrace s = do
  trace s ()
