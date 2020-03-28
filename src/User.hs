{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
module User (
              UserId
            , UserName (..)
            , UserPassword (..)
            , UserEmail (..)
            , UserSession
            , SessionId (..)
            , modifySessionRecord
            , logout
            , addSession
            , session
            , authUser
            ) where

-- External package
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Data.Text
import Data.Time.Clock
import Data.String
import Data.String.Conversions

import Models.User as M
import Common

newtype UserId    = UserId Int deriving (Eq, Show)
newtype UserName  = UserName Text deriving (Eq, Show)
newtype UserEmail = UserEmail Text deriving (Eq, Show)
newtype UserPassword = UserPassword Text deriving (Eq, Show)
newtype UserVerified = UserVerified Bool deriving (Eq, Show)

-- class DbField a where
--   dbField :: ToField b => a -> b

instance ToField UserName where toField (UserName x) = toField x
instance ToField UserPassword where toField (UserPassword x) = toField x

newtype SessionId = SessionId String deriving (Eq, Show)

data User = User {
    userId:: UserId
  , userName :: UserName
  , userEmail :: UserEmail
  , userPassword :: UserPassword
  , userVerified :: UserVerified
  } deriving (Eq, Show)

data CreateUserError
   = InvalidPassword
   | UsernameAlreadyTaken
   | EmailAlreadyTaken
   | UsernameAndEmailAlreadyTaken
   deriving (Show, Eq)

data UpdateUserError
   = UsernameAlreadyExists
   | EmailAlreadyExists
   | UserDoesntExist
   deriving (Show, Eq)

-- data SortBy t
--    = SortAsc t
--    | SortDesc t

-- class UserBackend b where
--   authUser :: b -> UserName -> UserPassword -> Bool

class UserSession sState sRecord | sState -> sRecord where
  addSession :: sState -> SessionId -> UTCTime -> sState
  logout :: sState -> SessionId -> sState
  session :: sState -> SessionId -> Maybe sRecord
  modifySessionRecord :: sState -> SessionId -> (Maybe sRecord -> sRecord) -> sState

authUser :: Connection -> UserName -> UserPassword -> IO Bool
authUser c u p =
  (dbSelect c $ addWhere (fromString $ cs ( sqlWhereEquals "userName" <> " AND " <> sqlWhereEquals "userPassword" :: Text)) (u, p) (modelDBSelect :: DBSelect M.User)) >>= \case
    (_:_) -> return True
    _ -> return False

