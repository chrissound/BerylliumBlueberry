{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module AppImage where

import AppCommon
import AppRender
import Control.Monad.IO.Class

import Models.Image as Image

import qualified Routes as R
import Template.Base
import Template.Image
import Template.AdminActions
import Forms.Comment
import ViewContext (fromSiteView)
import Models.Associations
import Lucid
import Database.PostgreSQL.Simple

listImage :: AppServer ()
listImage = do
  get (webRoute R.ListImage)$ do
    imagesViewPage
  get (webRoute $ R.ViewImage "imageId")$ do
    c <- liftAndCatchIO connection
    (param "imageId" >>= return . filterEid)
      >>=
      maybe404
        (\eid' ->
           (liftAndCatchIO $ getImageByEasyId eid' c)
            >>= do
            maybe404
              (\p -> withSvRenderPage (imageTitle p) (\sv -> imageView sv p))
        )

imagesViewPage :: AppAction ()
imagesViewPage = do
  c <- liftAndCatchIO connection
  images <- sortByDate <$> (liftAndCatchIO $ getAllImages c)
  withSvRenderPage "Gallery" (\sv -> let ctx = fromSiteView sv in renderCreateImageButton ctx <> imagesView sv images ctx)
  where
    getAllImages conn = query_ conn "SELECT \"imageId\", \"imageTitle\", \"imageEasyId\", \"imageCreated\", \"imageFile\" FROM \"image\"" :: IO [Image]
