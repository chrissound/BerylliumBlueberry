{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Models.ImageQueries where

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Data.Text (Text)
import Models.Image

-- | Get an image by its database ID
getImageById :: Int -> Connection -> IO (Maybe Image)
getImageById iid c = do
  let sql = [NI.text|
      SELECT "imageId", "imageTitle", "imageEasyId", "imageCreated", "thumbImageFile"
      FROM "image"
      WHERE "imageId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only iid) :: IO [Image]
  case results of
    (img:_) -> return $ Just img
    [] -> return Nothing

-- | Get an image by its easy ID (user-friendly URL slug)
getImageByEasyId :: Text -> Connection -> IO (Maybe Image)
getImageByEasyId eid c = do
  let sql = [NI.text|
      SELECT "imageId", "imageTitle", "imageEasyId", "imageCreated", "thumbImageFile"
      FROM "image"
      WHERE "imageEasyId" = ?
      |]
  results <- query c (fromString $ cs sql) (Only eid) :: IO [Image]
  case results of
    (r:_) -> return $ Just r
    [] -> return Nothing

-- | Get all images
getAllImages :: Connection -> IO [Image]
getAllImages c = do
  let sql = [NI.text|
      SELECT "imageId", "imageTitle", "imageEasyId", "imageCreated", "thumbImageFile"
      FROM "image"
      |]
  query_ c (fromString $ cs sql) :: IO [Image]

-- | Create a new image
createImage :: Image -> Connection -> IO ()
createImage img c = do
  let sql = [NI.text|
      INSERT INTO "image" ("imageId", "imageTitle", "imageEasyId", "imageCreated", "thumbImageFile")
      VALUES (?, ?, ?, ?, ?)
      |]
  _ <- execute c (fromString $ cs sql) img
  return ()

-- | Update an existing image
updateImage :: Image -> Connection -> IO ()
updateImage img c = do
  let sql = [NI.text|
      UPDATE "image"
      SET "imageTitle" = ?, "imageEasyId" = ?, "imageCreated" = ?, "thumbImageFile" = ?
      WHERE "imageId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (imageTitle img, imageEasyId img, imageCreated img, imageFile img, imageId img)
  return ()

-- | Delete an image by its ID
deleteImage :: Int -> Connection -> IO ()
deleteImage iid c = do
  let sql = [NI.text|
      DELETE FROM "image"
      WHERE "imageId" = ?
      |]
  _ <- execute c (fromString $ cs sql) (Only iid)
  return ()
