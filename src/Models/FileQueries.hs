{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.FileQueries where

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Data.Text (Text)
import Models.File

-- | Get a file by its database ID
getFileById :: Int -> Connection -> IO (Maybe File)
getFileById fid c = do
  let sql = [NI.text|
      SELECT "fileId", "fileTitle", "fileEasyId", "fileCreated", "fileFile"
      FROM "file"
      WHERE "fileId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only fid) :: IO [File]
  case results of
    (f:_) -> return $ Just f
    [] -> return Nothing

-- | Get a file by its easy ID (user-friendly URL slug)
getFileByEasyId :: Text -> Connection -> IO (Maybe File)
getFileByEasyId eid c = do
  let sql = [NI.text|
      SELECT "fileId", "fileTitle", "fileEasyId", "fileCreated", "fileFile"
      FROM "file"
      WHERE "fileEasyId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only eid) :: IO [File]
  case results of
    (r:_) -> return $ Just r
    [] -> return Nothing

-- | Get all files
getAllFiles :: Connection -> IO [File]
getAllFiles c = do
  let sql = [NI.text|
      SELECT "fileId", "fileTitle", "fileEasyId", "fileCreated", "fileFile"
      FROM "file"
      |]
  query_ c (fromString $ cs sql) :: IO [File]

-- | Create a new file
createFile :: File -> Connection -> IO ()
createFile f c = do
  let sql = [NI.text|
      INSERT INTO "file" ("fileId", "fileTitle", "fileEasyId", "fileCreated", "fileFile")
      VALUES (?, ?, ?, ?, ?)
      |]
  _ <- execute c (fromString $ cs sql) f
  return ()

-- | Update an existing file
updateFile :: File -> Connection -> IO ()
updateFile f c = do
  let sql = [NI.text|
      UPDATE "file"
      SET "fileTitle" = ?, "fileEasyId" = ?, "fileCreated" = ?, "fileFile" = ?
      WHERE "fileId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (fileTitle f, fileEasyId f, fileCreated f, fileFile f, fileId f)
  return ()

-- | Delete a file by its ID
deleteFile :: Int -> Connection -> IO ()
deleteFile fid c = do
  let sql = [NI.text|
      DELETE FROM "file"
      WHERE "fileId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only fid)
  return ()
