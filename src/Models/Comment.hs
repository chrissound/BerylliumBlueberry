{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.Comment where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Time
import Data.Text
import GHC.Generics

data Comment = Comment {
    commentId :: Int
  , postId :: Int  -- Foreign key to Post
  , commentBody :: Text
  , authorAlias :: Text
  , approved :: Bool
  , postCreated :: UTCTimestamp
  } deriving (Generic, Show)

instance FromRow Comment
instance ToRow Comment
