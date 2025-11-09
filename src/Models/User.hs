{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.User where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Text
import GHC.Generics

data User = User {
    userId:: Int
  , userName :: Text
  , userEmail :: Text
  , userPassword :: Text
  , userVerified :: Bool
  } deriving (Generic, Show)

instance FromRow User
instance ToRow User
