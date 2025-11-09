{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.PostType where

import Models.Post
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics
import Data.Maybe
import Database
import DatastoreHelper
import Common
import Data.List
import Data.Function
import qualified NeatInterpolation as NI
import Data.String.Conversions
import Data.String

data PostType = PostType {
    postTypeId :: Int
  , postId :: Int  -- Foreign key to Post
  , postType :: Int
  } deriving (Generic, Show)

instance FromRow PostType
instance ToRow PostType

data PostTypeEnum = PostTypeBlog | PostTypePage deriving (Enum, Show, Eq)

data PagePost = PagePost {
    postTypeV :: PostType
  , postV :: Post
  } deriving Show

getPagePosts :: IO [PagePost]
getPagePosts = do
  c <- connection
  let sql = [NI.text|
      SELECT "postTypeId", "postId", "postType"
      FROM "postType"
      WHERE "postType" = ?
      |]
  postTypes <- query c (fromString $ cs sql) (Only $ fromEnum PostTypePage) :: IO [PostType]

  let sql2 = [NI.text|
      SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
      FROM "post"
      |]
  posts <- query_ c (fromString $ cs sql2) :: IO [Post]

  pure $ sortByDate $ catMaybes $ combineByIntIndex
            PagePost
            (Models.PostType.postId)
            (Models.Post.postId)
            postTypes
            posts

getPagePosts' :: PostTypeEnum -> IO [PagePost]
getPagePosts' x = do
  c <- connection
  let sql = [NI.text|
      SELECT "postTypeId", "postId", "postType"
      FROM "postType"
      WHERE "postType" = ?
      |]
  postTypes <- query c (fromString $ cs sql) (Only $ fromEnum x) :: IO [PostType]

  let sql2 = [NI.text|
      SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
      FROM "post"
      ORDER BY "postCreated" DESC
      |]
  posts <- query_ c (fromString $ cs sql2) :: IO [Post]

  pure $ sortByDate $ catMaybes $ combineByIntIndex
            PagePost
            (Models.PostType.postId)
            (Models.Post.postId)
            postTypes
            posts

sortByDate :: [PagePost] -> [PagePost]
sortByDate = reverse . sortBy (compare `on` (postCreated . postV ))
