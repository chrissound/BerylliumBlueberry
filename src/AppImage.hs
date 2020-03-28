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
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model
import Template.Base
import Template.Image
import Forms.Comment
import Models.Associations
import Lucid

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
  images <- sortByDate <$> (liftAndCatchIO $ findAll c)
  (imageHook, preHook) <- AppCommon.session >>= \case
    Just _ -> return (imageViewExtraAdmin, imagesViewExtraAdmin)
    Nothing -> return (constHtml, return ())
  withSvRenderPage "Gallery" (\sv -> preHook <> imagesView sv images imageHook)
