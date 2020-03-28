{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Post where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Time
import Data.Char
import Data.Text
import GHC.Generics
import Data.String.Conversions
import Data.String

import Common

data Post = Post {
    postId :: DBKey
  , postTitle :: Text
  , postBody :: Text
  , postCreated :: UTCTimestamp
  , postEasyId :: Text
  } deriving (Generic, Show)

instance Model Post

filterEid :: Text -> Maybe Text
filterEid s = case (Prelude.all (== True) $ Prelude.map (\x -> or [isAlphaNum x, x == '-']) $ cs s) of
  True -> Just s
  False -> Nothing


getPostByEasyId :: Text -> Connection -> IO (Maybe Post)
getPostByEasyId eid c = do
    z <- dbSelect c $ (addWhere (fromString $ cs $ sqlWhereEquals "postEasyId") (Only eid) modelDBSelect )
    case z of
      (r:_) -> return $ Just r
      (_) -> return Nothing
