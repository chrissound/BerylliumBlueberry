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
import Forms.Comment
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
  (fileHook, preHook) <- AppCommon.session >>= \case
    Just _ -> return (fileViewExtraAdmin, filesViewExtraAdmin)
    Nothing -> return (constHtml, return ())
  files <- sortByDate <$> (liftAndCatchIO $ getAllFiles c)
  withSvRenderPage "Files" (\sv -> preHook <> filesView sv files fileHook)
  where
    getAllFiles conn = query_ conn "SELECT \"fileId\", \"fileTitle\", \"fileEasyId\", \"fileCreated\", \"fileFile\" FROM \"file\"" :: IO [File]
