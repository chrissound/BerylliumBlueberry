{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Template.Image where

import Control.Monad
import qualified Data.Text as T
import Data.String.Conversions
import Data.String
import Database.PostgreSQL.Simple.Time
import Data.Time.Format
import Text.Pandoc
import Control.Monad.Except
import Control.Monad.Trans.State.Strict
import Lucid

import qualified Models.Image as M
import qualified Routes as R
import Common
import Template.Types
import Template.AdminActions
import AppTypes
import ViewContext

imageView :: SiteView -> M.Image -> Html ()
imageView sv image = do
  div_ [ class_ "row" ] $ do
    p_ $ fromString $ show image
    div_ [ class_ "col-sm-8 gallery-image" ] $ do
      h1_ [ class_ "blog-image-title" ] $ fromString $ cs $ M.imageTitle image
      img_ [ src_ $ cs $ "/data/uploads/image/" ++ (fileSource)]
    sideBarView sv
  where
    fileSource = (\(ImageResizedFileUpload x) -> x) $ M.imageFile image

imagesView :: SiteView -> [M.Image] -> ViewContext -> Html ()
imagesView sv images ctx = do
  basicContent sv "Gallery" $ do
    with table_ [class_ "imagesTable"] $ do
      forM_ images $ \image -> do
        let fileSource = (\(ImageResizedFileUpload x) -> x) $ M.imageFile image
        tr_ $ do
          td_ $ pure ()
          td_ $ do
            with a_ [ href_ (fromString $ R.renderStrPublicUrl $ R.ViewImage $ cs $ M.imageEasyId image) ] $ fromString $ cs $ M.imageTitle image
        with tr_ [class_ "xyz"] $ do
          with td_ [class_ "date"] $ do
            renderImageActions ctx image
            span_ (fromString $ formatUTC $ M.imageCreated image) -- <> " "
          with td_ [class_ "last"] $ do
            with a_ [ href_ (fromString $ R.renderStrPublicUrl $ R.ViewImage $ cs $ M.imageEasyId image) ]
              $ img_ [ src_ $ cs $ "/data/uploads/imagethumb/" ++ (fileSource)]
      p_ ""
