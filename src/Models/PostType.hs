{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.PostType where

import Models.Post
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple
import GHC.Generics
import Data.Maybe
import Database
import DatastoreHelper
import Common
import Data.List
import Data.Function

data PostType = PostType {
    postTypeId :: DBKey
  , postId :: DBRef Post
  , postType :: Int
  } deriving (Generic, Show)

instance Model PostType

data PostTypeEnum = PostTypeBlog | PostTypePage deriving (Enum, Show, Eq)

data PagePost = PagePost {
    postTypeV :: PostType
  , postV :: Post
  } deriving Show

getPagePosts :: IO [PagePost]
getPagePosts = do
  c <- connection
  postTypes <- (dbSelect c $ addWhere "\"postType\" = ?" (Only $ fromEnum PostTypePage) (modelDBSelect :: DBSelect PostType))
  posts <- (dbSelect c (modelDBSelect :: DBSelect Post))

  pure $ sortByDate $ catMaybes $ combineByIntIndex
            PagePost
            (idIntegerRef . Models.PostType.postId)
            (idInteger . Models.Post.postId)
            postTypes
            posts

getPagePosts' :: PostTypeEnum -> IO [PagePost]
getPagePosts' x = do
  c <- connection
  postTypes <-
    (
      dbSelect c
      $ addWhere "\"postType\" = ?" (Only $ fromEnum x)
        (modelDBSelect :: DBSelect PostType)
    )
  posts <- dbSelect c
    ((modelDBSelect :: DBSelect Post)
      {
        selOrderBy = "ORDER BY \"postCreated\" desc" 
      }
    )

  pure $ sortByDate $ catMaybes $ combineByIntIndex
            PagePost
            (idIntegerRef . Models.PostType.postId)
            (idInteger . Models.Post.postId)
            postTypes
            posts

sortByDate :: [PagePost] -> [PagePost]
sortByDate = reverse . sortBy (compare `on` (postCreated . postV ))
