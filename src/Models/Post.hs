{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.Post where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
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
import qualified NeatInterpolation as NI

data Post = Post {
    postId :: Int
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

instance FromRow Post
instance ToRow Post

filterEid :: Text -> Maybe Text
filterEid s = case (Prelude.all (== True) $ Prelude.map (\x -> or [isAlphaNum x, x == '-']) $ cs s) of
  True -> Just s
  False -> Nothing


getPostByEasyId :: Text -> Connection -> IO (Maybe Post)
getPostByEasyId eid c = do
    let sql = [NI.text|
        SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
        FROM "post"
        WHERE "postEasyId" = ?
        |]
    z <- query c (fromString $ cs sql) (Only eid) :: IO [Post]
    case z of
      (r:_) -> return $ Just r
      (_) -> return Nothing
