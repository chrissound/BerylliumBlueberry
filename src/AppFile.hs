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
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model
import Template.Base
import Template.File
import Forms.Comment
import Models.Associations
import Lucid
import Network.HTTP.Types

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
  files <- sortByDate <$> (liftAndCatchIO $ findAll c)
  withSvRenderPage "Files" (\sv -> preHook <> filesView sv files fileHook)
