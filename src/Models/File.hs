{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wno-unused-imports #-}

module Models.File where

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
import AppCommon hiding (File, files)
import System.Directory
import AppTypeDb
import Data.List
import Data.Function
import qualified NeatInterpolation as NI


data File = File {
    fileId :: Int
  , fileTitle :: Text
  , fileEasyId :: Text
  , fileCreated :: UTCTimestamp
  , fileFile :: FileUpload
  } deriving (Generic, Show)

instance FromRow File
instance ToRow File

filterEid :: Text -> Maybe Text
filterEid s = case (Prelude.all (== True) $ Prelude.map (\x -> or [isAlphaNum x, x == '-']) $ cs s) of
  True -> Just s
  False -> Nothing


getFileByEasyId :: Text -> Connection -> IO (Maybe File)
getFileByEasyId eid c = do
    let sql = [NI.text|
        SELECT "fileId", "fileTitle", "fileEasyId", "fileCreated", "fileFile"
        FROM "file"
        WHERE "fileEasyId" = ?
        |]
    z <- query c (fromString $ cs sql) (Only eid) :: IO [File]
    case z of
      (r:_) -> return $ Just r
      (_) -> return Nothing


fileDataPath :: AppAction FilePath
fileDataPath = do
  x <- appIO getCurrentDirectory
  pure $ x ++ "/files"

sortByDate :: [File] -> [File]
sortByDate = Prelude.reverse . sortBy (compare `on` (fileCreated))
