{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module AppFile where

import AppCommon hiding (File, files)
import AppRender
import Control.Monad.IO.Class

import Models.File as File

import qualified Routes as R
import Template.Base
import Template.File
import Template.AdminActions
import Forms.Comment
import ViewContext (fromSiteView)
import Models.Associations
import Lucid
import Network.HTTP.Types
import Database.PostgreSQL.Simple

listFile :: AppServer ()
listFile = do
  get (webRoute R.ListFile)$ do
    filesViewPage
  -- get (webRoute $ R.DownloadFile "fileId")$ do
  --   c <- liftAndCatchIO connection
  --   fdp <- fileDataPath
  --   param "fileId"
  --     >>= liftAndCatchIO . flip getFileByEasyId c
  --     >>= pure . (\case
  --             Just x' -> Just ((\(FileUploadName x'') -> fdp ++ "/" ++ x'') . fileFile $ x')
  --             Nothing -> Nothing )
  --     >>= maybe404 file

filesViewPage :: AppAction ()
filesViewPage = do
  c <- liftAndCatchIO connection
  files <- sortByDate <$> (liftAndCatchIO $ getAllFiles c)
  withSvRenderPage "Files" (\sv -> let ctx = fromSiteView sv in renderCreateFileButton ctx <> filesView sv files ctx)
  where
    getAllFiles conn = query_ conn "SELECT \"fileId\", \"fileTitle\", \"fileEasyId\", \"fileCreated\", \"fileFile\" FROM \"file\"" :: IO [File]
