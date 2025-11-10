{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.PostQueries where

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Data.Text (Text)
import Models.Post

-- | Get a post by its database ID
getPostById :: Int -> Connection -> IO (Maybe Post)
getPostById pid c = do
  let sql = [NI.text|
      SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
      FROM "post"
      WHERE "postId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only pid) :: IO [Post]
  case results of
    (p:_) -> return $ Just p
    [] -> return Nothing

-- | Get a post by its easy ID (user-friendly URL slug)
getPostByEasyId :: Text -> Connection -> IO (Maybe Post)
getPostByEasyId eid c = do
  let sql = [NI.text|
      SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
      FROM "post"
      WHERE "postEasyId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only eid) :: IO [Post]
  case results of
    (r:_) -> return $ Just r
    [] -> return Nothing

-- | Get all posts
getAllPosts :: Connection -> IO [Post]
getAllPosts c = do
  let sql = [NI.text|
      SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
      FROM "post"
      |]
  query_ c (fromString $ cs sql) :: IO [Post]

-- | Create a new post and return its generated ID
createPost :: Post -> Connection -> IO Int
createPost p c = do
  let sql = [NI.text|
      INSERT INTO "post" ("postTitle", "postBody", "postCreated", "postEasyId", "postTags")
      VALUES (?, ?, ?, ?, ?)
      RETURNING "postId"
      |]
  results <- query c (fromString $ cs sql) (postTitle p, postBody p, postCreated p, postEasyId p, postTags p) :: IO [Only Int]
  case results of
    (Only pid:_) -> return pid
    [] -> error "Failed to create post"

-- | Update an existing post
updatePost :: Post -> Connection -> IO ()
updatePost p c = do
  let sql = [NI.text|
      UPDATE "post"
      SET "postTitle" = ?, "postBody" = ?, "postCreated" = ?, "postEasyId" = ?, "postTags" = ?
      WHERE "postId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (postTitle p, postBody p, postCreated p, postEasyId p, postTags p, postId p)
  return ()

-- | Delete a post by its ID
deletePost :: Int -> Connection -> IO ()
deletePost pid c = do
  let sql = [NI.text|
      DELETE FROM "post"
      WHERE "postId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only pid)
  return ()
