{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.UserQueries where

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Data.Text (Text)
import Models.User

-- | Get a user by their ID
getUserById :: Int -> Connection -> IO (Maybe User)
getUserById uid c = do
  let sql = [NI.text|
      SELECT "userId", "userName", "userEmail", "userPassword", "userVerified"
      FROM "user"
      WHERE "userId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only uid) :: IO [User]
  case results of
    (u:_) -> return $ Just u
    [] -> return Nothing

-- | Get a user by their username
getUserByName :: Text -> Connection -> IO (Maybe User)
getUserByName uname c = do
  let sql = [NI.text|
      SELECT "userId", "userName", "userEmail", "userPassword", "userVerified"
      FROM "user"
      WHERE "userName" = ?
      |]
  results <- query c (fromString $ cs sql) (Only uname) :: IO [User]
  case results of
    (u:_) -> return $ Just u
    [] -> return Nothing

-- | Get a user by their email
getUserByEmail :: Text -> Connection -> IO (Maybe User)
getUserByEmail uemail c = do
  let sql = [NI.text|
      SELECT "userId", "userName", "userEmail", "userPassword", "userVerified"
      FROM "user"
      WHERE "userEmail" = ?
      |]
  results <- query c (fromString $ cs sql) (Only uemail) :: IO [User]
  case results of
    (u:_) -> return $ Just u
    [] -> return Nothing

-- | Authenticate a user with username and password
authenticateUser :: Text -> Text -> Connection -> IO Bool
authenticateUser uname upass c = do
  let sql = [NI.text|
      SELECT "userId", "userName", "userEmail", "userPassword", "userVerified"
      FROM "user"
      WHERE "userName" = ? AND "userPassword" = ?
      |]
  results <- query c (fromString $ cs sql) (uname, upass) :: IO [User]
  case results of
    (_:_) -> return True
    [] -> return False

-- | Get all users
getAllUsers :: Connection -> IO [User]
getAllUsers c = do
  let sql = [NI.text|
      SELECT "userId", "userName", "userEmail", "userPassword", "userVerified"
      FROM "user"
      |]
  query_ c (fromString $ cs sql) :: IO [User]

-- | Create a new user
createUser :: User -> Connection -> IO ()
createUser user c = do
  let sql = [NI.text|
      INSERT INTO "user" ("userId", "userName", "userEmail", "userPassword", "userVerified")
      VALUES (?, ?, ?, ?, ?)
      |]
  _ <- execute c (fromString $ cs sql) user
  return ()

-- | Update an existing user
updateUser :: User -> Connection -> IO ()
updateUser user c = do
  let sql = [NI.text|
      UPDATE "user"
      SET "userName" = ?, "userEmail" = ?, "userPassword" = ?, "userVerified" = ?
      WHERE "userId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (userName user, userEmail user, userPassword user, userVerified user, userId user)
  return ()

-- | Verify a user
verifyUser :: Int -> Connection -> IO ()
verifyUser uid c = do
  let sql = [NI.text|
      UPDATE "user"
      SET "userVerified" = true
      WHERE "userId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only uid)
  return ()

-- | Delete a user by their ID
deleteUser :: Int -> Connection -> IO ()
deleteUser uid c = do
  let sql = [NI.text|
      DELETE FROM "user"
      WHERE "userId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only uid)
  return ()
