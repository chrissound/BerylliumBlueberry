{-# LANGUAGE OverloadedStrings #-}
module Models.Associations where

import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple

import Models.Post
import Models.Comment

postComment :: Association Post Comment
postComment = has

approvedPostComment :: Models.Post.Post -> DBSelect Models.Comment.Comment
approvedPostComment p = addWhere "approved = ?" (Only True) (assocWhere postComment p)
