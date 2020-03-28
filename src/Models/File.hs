{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-unused-imports #-}

module Models.File where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple.Time
import Data.Char
import Data.Text
import GHC.Generics
import Data.String.Conversions
import Data.String
import AppTypes
import AppCommon hiding (File, files)
import System.Directory
import AppTypeDb
import Data.List
import Data.Function


data File = File {
    fileId :: DBKey
  , fileTitle :: Text
  , fileEasyId :: Text
  , fileCreated :: UTCTimestamp
  , fileFile :: FileUploadName
  } deriving (Generic, Show)

instance Model File

filterEid :: Text -> Maybe Text
filterEid s = case (Prelude.all (== True) $ Prelude.map (\x -> or [isAlphaNum x, x == '-']) $ cs s) of
  True -> Just s
  False -> Nothing


getFileByEasyId :: Text -> Connection -> IO (Maybe File)
getFileByEasyId eid c = do
    z <- dbSelect c $ (addWhere (fromString $ cs $ sqlWhereEquals "fileEasyId") (Only eid) modelDBSelect )
    case z of
      (r:_) -> return $ Just r
      (_) -> return Nothing


fileDataPath :: AppAction FilePath
fileDataPath = do
  x <- appIO getCurrentDirectory
  pure $ x ++ "/files"

sortByDate :: [File] -> [File]
sortByDate = Prelude.reverse . sortBy (compare `on` (fileCreated))
