{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Models.User where 

import Database.PostgreSQL.ORM
import Data.Text
import GHC.Generics

data User = User {
    userId:: DBKey
  , userName :: Text
  , userEmail :: Text
  , userPassword :: Text
  , userVerified :: Bool
  } deriving (Generic, Show)
instance Model User

