{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.Comment where 

import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Time
import Data.Text
import GHC.Generics
import Models.Post

data Comment = Comment {
    commentId :: DBKey
  , postId :: DBRef Post
  , commentBody :: Text
  , authorAlias :: Text
  , approved :: Bool
  , postCreated :: UTCTimestamp
  } deriving (Generic, Show)
instance Model Comment
