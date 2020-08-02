module Common (
    module Common
  , module Control.Monad.IO.Class
  ) where

import Database.PostgreSQL.ORM.Model
import Data.Text
import Data.String.Conversions
import Text.Printf
import Debug.Trace
import Control.Monad.IO.Class
import Text.Pretty.Simple

idInteger :: DBKey -> Int
idInteger (DBKey x ) = fromIntegral x
idInteger (NullKey) = error "This should never occur..."

idIntegerRef :: DBRef a -> Int
idIntegerRef (DBRef x ) = fromIntegral x

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
