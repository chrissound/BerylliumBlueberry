{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.PostTypeQueries where

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Models.PostType

-- | Get a PostType by its ID
getPostTypeById :: Int -> Connection -> IO (Maybe PostType)
getPostTypeById ptid c = do
  let sql = [NI.text|
      SELECT "postTypeId", "postId", "postType"
      FROM "postType"
      WHERE "postTypeId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only ptid) :: IO [PostType]
  case results of
    (pt:_) -> return $ Just pt
    [] -> return Nothing

-- | Get the PostType for a specific post by post ID
getPostTypeByPostId :: Int -> Connection -> IO (Maybe PostType)
getPostTypeByPostId pid c = do
  let sql = [NI.text|
      SELECT "postTypeId", "postId", "postType"
      FROM "postType"
      WHERE "postId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only pid) :: IO [PostType]
  case results of
    (pt:_) -> return $ Just pt
    [] -> return Nothing

-- | Get all PostTypes for a specific type (Blog, Page, etc.)
getPostTypesByType :: PostTypeEnum -> Connection -> IO [PostType]
getPostTypesByType typeEnum c = do
  let sql = [NI.text|
      SELECT "postTypeId", "postId", "postType"
      FROM "postType"
      WHERE "postType" = ?
      |]
  query c (fromString $ cs sql) (Only $ fromEnum typeEnum) :: IO [PostType]

-- | Create a new PostType
createPostType :: PostType -> Connection -> IO ()
createPostType pt c = do
  let sql = [NI.text|
      INSERT INTO "postType" ("postId", "postType")
      VALUES (?, ?)
      |]
  _ <- execute c (fromString $ cs sql) (postId pt, postType pt)
  return ()

-- | Update an existing PostType
updatePostType :: PostType -> Connection -> IO ()
updatePostType pt c = do
  let sql = [NI.text|
      UPDATE "postType"
      SET "postId" = ?, "postType" = ?
      WHERE "postTypeId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (postId pt, postType pt, postTypeId pt)
  return ()

-- | Delete a PostType by its ID
deletePostTypeById :: Int -> Connection -> IO ()
deletePostTypeById ptid c = do
  let sql = [NI.text|
      DELETE FROM "postType"
      WHERE "postTypeId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only ptid)
  return ()

-- | Delete a PostType by the associated post ID
deletePostTypeByPostId :: Int -> Connection -> IO ()
deletePostTypeByPostId pid c = do
  let sql = [NI.text|
      DELETE FROM "postType"
      WHERE "postId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only pid)
  return ()
