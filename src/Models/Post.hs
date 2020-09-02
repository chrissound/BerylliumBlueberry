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
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField
import Data.ByteString.Lazy
import Common
import Data.Aeson
import Data.Aeson.Types

data Post = Post {
    postId :: DBKey
  , postTitle :: Text
  , postBody :: Text
  , postCreated :: UTCTimestamp
  , postEasyId :: Text
  , postTags :: PostTags
  } deriving (Generic, Show)

data PostTags = PostTags [Text] deriving Show

getPostTags :: PostTags -> [Text]
getPostTags (PostTags x) = x

instance FromField PostTags where
   fromField f x = do
     parseMaybe parseJSON <$> fromField f x >>= \case
       Just m' -> pure $ PostTags m'
       Nothing -> returnError Incompatible f "PostTags ???"
instance ToField PostTags where
  toField (PostTags x) = toField $ encode x

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
