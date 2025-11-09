{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wno-unused-imports #-}

module Models.Image where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Time
import Data.Char
import Data.Text
import GHC.Generics
import Data.String.Conversions
import Data.String
import AppTypes
import AppCommon
import System.Directory
import AppTypeDb
import Data.List
import Data.Function
import Models.File
import qualified NeatInterpolation as NI


data Image = Image {
    imageId :: Int
  , imageTitle :: Text
  , imageEasyId :: Text
  , imageCreated :: UTCTimestamp
  , imageFile :: ImageResizedFileUpload
  } deriving (Generic, Show)

instance FromRow Image
instance ToRow Image

imageToFile :: Image -> Models.File.File
imageToFile (Image a b c d (ImageResizedFileUpload e)) = File a b c d (FileUpload e)

filterEid :: Text -> Maybe Text
filterEid s = case (Prelude.all (== True) $ Prelude.map (\x -> or [isAlphaNum x, x == '-']) $ cs s) of
  True -> Just s
  False -> Nothing


getImageByEasyId :: Text -> Connection -> IO (Maybe Image)
getImageByEasyId eid c = do
    let sql = [NI.text|
        SELECT "imageId", "imageTitle", "imageEasyId", "imageCreated", "imageFile"
        FROM "image"
        WHERE "imageEasyId" = ?
        |]
    z <- query c (fromString $ cs sql) (Only eid) :: IO [Image]
    case z of
      (r:_) -> return $ Just r
      (_) -> return Nothing


imageDataPath :: AppAction FilePath
imageDataPath = do
  x <- appIO getCurrentDirectory
  pure $ x ++ "/data/uploads/image"


imageThumbDataPath :: AppAction FilePath
imageThumbDataPath = do
  x <- appIO getCurrentDirectory
  pure $ x ++ "/data/uploads/imagethumb"

sortByDate :: [Image] -> [Image]
sortByDate = Prelude.reverse . sortBy (compare `on` (imageCreated))
