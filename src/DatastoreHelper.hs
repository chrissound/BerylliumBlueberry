module DatastoreHelper where

import Prelude hiding (lookup)
import Data.IntMap.Strict

combineByIntIndex :: (a -> b -> c) -> (a -> Int) -> (b -> Int) -> [a] -> [b] -> [Maybe c]
combineByIntIndex f ai bi a b = do
  let bmapped = fromList $ (\b' -> (bi b', b')) <$> b
  fmap (\a' -> f a' <$> (lookup (ai a') bmapped)) a
