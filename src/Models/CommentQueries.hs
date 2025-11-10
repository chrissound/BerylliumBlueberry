{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.CommentQueries where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Data.Text (Text)
import Models.Comment

-- | Get a comment by its ID
getCommentById :: Int -> Connection -> IO (Maybe Comment)
getCommentById cid c = do
  let sql = [NI.text|
      SELECT "commentId", "postId", "commentBody", "authorAlias", "approved", "postCreated"
      FROM "comment"
      WHERE "commentId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only cid) :: IO [Comment]
  case results of
    (comment:_) -> return $ Just comment
    [] -> return Nothing

-- | Get all approved comments for a specific post
getApprovedCommentsForPost :: Int -> Connection -> IO [Comment]
getApprovedCommentsForPost pid c = do
  let sql = [NI.text|
      SELECT "commentId", "postId", "commentBody", "authorAlias", "approved", "postCreated"
      FROM "comment"
      WHERE "postId" = ? AND "approved" = ?
      |]
  query c (fromString $ cs sql) (pid, True) :: IO [Comment]

-- | Get all comments (approved and unapproved) for a specific post
getAllCommentsForPost :: Int -> Connection -> IO [Comment]
getAllCommentsForPost pid c = do
  let sql = [NI.text|
      SELECT "commentId", "postId", "commentBody", "authorAlias", "approved", "postCreated"
      FROM "comment"
      WHERE "postId" = ?
      |]
  query c (fromString $ cs sql) (Only pid) :: IO [Comment]

-- | Get all comments with their associated post titles (for admin view)
listAllCommentsWithPostTitles :: Connection -> IO [(Int, Int, Text, Text, Bool, UTCTimestamp, Text)]
listAllCommentsWithPostTitles c = do
  let sql = [NI.text|
      SELECT c."commentId", c."postId", c."commentBody", c."authorAlias", c."approved", c."postCreated", p."postTitle"
      FROM "comment" c
      JOIN "post" p ON c."postId" = p."postId"
      ORDER BY c."postCreated" DESC
      |]
  query_ c (fromString $ cs sql) :: IO [(Int, Int, Text, Text, Bool, UTCTimestamp, Text)]

-- | Create a new comment
createComment :: Comment -> Connection -> IO ()
createComment comment c = do
  let sql = [NI.text|
      INSERT INTO "comment" ("postId", "commentBody", "authorAlias", "approved", "postCreated")
      VALUES (?, ?, ?, ?, ?)
      |]
  _ <- execute c (fromString $ cs sql) (postId comment, commentBody comment, authorAlias comment, approved comment, postCreated comment)
  return ()

-- | Approve a comment (set approved = true)
approveComment :: Int -> Connection -> IO ()
approveComment cid c = do
  let sql = [NI.text|
      UPDATE "comment"
      SET "approved" = true
      WHERE "commentId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only cid)
  return ()

-- | Disapprove a comment (set approved = false)
disapproveComment :: Int -> Connection -> IO ()
disapproveComment cid c = do
  let sql = [NI.text|
      UPDATE "comment"
      SET "approved" = false
      WHERE "commentId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only cid)
  return ()

-- | Delete a comment by its ID
deleteComment :: Int -> Connection -> IO ()
deleteComment cid c = do
  let sql = [NI.text|
      DELETE FROM "comment"
      WHERE "commentId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only cid)
  return ()
