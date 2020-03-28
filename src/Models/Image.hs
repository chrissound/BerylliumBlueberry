{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-unused-imports #-}

module Models.Image where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM
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


data Image = Image {
    imageId :: DBKey
  , imageTitle :: Text
  , imageEasyId :: Text
  , imageCreated :: UTCTimestamp
  , imageFile :: FileUploadName
  } deriving (Generic, Show)

instance Model Image

filterEid :: Text -> Maybe Text
filterEid s = case (Prelude.all (== True) $ Prelude.map (\x -> or [isAlphaNum x, x == '-']) $ cs s) of
  True -> Just s
  False -> Nothing


getImageByEasyId :: Text -> Connection -> IO (Maybe Image)
getImageByEasyId eid c = do
    z <- dbSelect c $ (addWhere (fromString $ cs $ sqlWhereEquals "imageEasyId") (Only eid) modelDBSelect )
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
