{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-local-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Template.File where

import Control.Monad
import qualified Data.Text as T
import Data.String.Conversions
import Data.String
import Database.PostgreSQL.ORM (DBKey)
import Database.PostgreSQL.Simple.Time
import Data.Time.Format
import Text.Pandoc
import Control.Monad.Except
import Control.Monad.Trans.State.Strict
import Lucid

import qualified Models.User as M
import qualified Models.File as M
import qualified Routes as R
import Common
import Template.Types
import AppTypes

fileView :: SiteView -> M.File -> Html ()
fileView sv file = do
  div_ [ class_ "row" ] $ do
    div_ [ class_ "col-sm-8 gallery-file" ] $ do
      h1_ [ class_ "blog-file-title" ] $ fromString $ cs $ M.fileTitle file
      img_ [ src_ $ cs $ "/data/uploads/file/" ++ (fileSource)]
    sideBarView sv
  where
    fileSource = (\(FileUploadName x) -> x) $ M.fileFile file

filesView :: SiteView -> [M.File] -> (M.File -> Html ()) -> Html ()
filesView sv files fileHook = do
  basicContent sv "Files" $ do
    with table_ [class_ "filesTable"] $ do
      forM_ files $ \file -> do
        let fileSource = (\(FileUploadName x) -> x) $ M.fileFile file
        tr_ $ do
          td_ $ pure ()
          td_ $ do
            with a_ [ href_ (fromString $ R.renderStrPublicUrl $ R.DownloadFile $ (\(FileUploadName x) -> x) $ M.fileFile file) ] $ fromString $ cs $ M.fileTitle file
        with tr_ [class_ "xyz"] $ do
          with td_ [class_ "date"] $ do
            fileHook file
            span_ (fromString $ formatUTC $ M.fileCreated file) -- <> " "
          with td_ [class_ "last"] $ do
            pure ()
            -- with a_ [ href_ (fromString $ R.renderStrPublicUrl $ R.ViewFile $ cs $ M.fileEasyId file) ]
            --   $ img_ [ src_ $ cs $ "/data/uploads/filethumb/" ++ (fileSource)]
      p_ ""

fileViewExtraAdmin :: M.File -> Html ()
fileViewExtraAdmin p = do
  buttonConfirmView  (renderParamUrl R.AdminDeleteFile $ M.fileId p) "" "Delete" AlertRed ExtraSmall
  fromString " "
  buttonView (renderParamUrl R.AdminEditFile $ M.fileId p) "" "Edit" InfoBlue ExtraSmall
  fromString " "

filesViewExtraAdmin :: Html ()
filesViewExtraAdmin = do
  br_ []
  buttonView (R.renderPublicUrl R.AdminCreateFile) "" "Create File" InfoBlue Normal
