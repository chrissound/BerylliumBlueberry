{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Models.Associations where

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI

import Models.Post
import Models.Comment

getApprovedPostComments :: Connection -> Models.Post.Post -> IO [Models.Comment.Comment]
getApprovedPostComments c p = do
  let sql = [NI.text|
      SELECT "commentId", "postId", "commentBody", "authorAlias", "approved", "postCreated"
      FROM "comment"
      WHERE "postId" = ? AND "approved" = ?
      |]
  query c (fromString $ cs sql) (Models.Post.postId p, True)
