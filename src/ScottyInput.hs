module ScottyInput where

import AppTypes
import Data.String.Conversions
import Web.Scotty.Trans
import Data.Map

scottyFormInput :: AppAction [(String, String)]
scottyFormInput = do
  x <- fmap (\(a,b) -> (cs a, cs b)) <$> params
  y <- (fmap (\(y',_) -> (cs y', cs y'))) <$> files
  pure (x ++ y)
