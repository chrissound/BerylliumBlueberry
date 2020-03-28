module ScottyInput where

import AppTypes
import Data.String.Conversions
import Web.Scotty.Trans
import Data.Map

scottyFormInput :: AppAction [(String, String)]
scottyFormInput = removeX <$> fmap (\(a,b) -> (cs a :: String, cs b :: String)) <$> params

removeX :: Ord t => [(t, s)] -> [(t, s)]
removeX [] = []
removeX a = toList $ fromList a
