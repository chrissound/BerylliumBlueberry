{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wno-unused-imports #-}

module AppAdminGalleryImage where

import Data.Map as Map
import qualified Routes as R
import Web.Scotty.Trans
import Web.Cookie
import Web.Scotty.Cookie
import System.Entropy
import Control.Monad.Reader
import Blog

import Data.Time.Clock
import Data.Text (Text, map)
import Data.Bool

import Lucid

import Forms.Image
import Forms.File (postForm, postForm')
import NioForm
import NioFormExtra
import NioFormTypes

import Models.Image as Image
import Models.ImageQueries as ImageQueries
import User
import Database

import Data.ByteString.Base64 as B64
import Template.Base

import ScottyInput

import AppRender
import AppCommon
import AppPost
import AppAdminSinglePage
import AdminSettings
import Text.Pretty.Simple

import Data.List.Utils (replace)
import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import qualified MyNioFieldError as MNE

getImageElseError :: Int -> AppAction Image
getImageElseError x = do
  c <- liftAndCatchIO connection
  mImage <- liftAndCatchIO $ ImageQueries.getImageById x c
  case mImage of
    Just img -> return img
    Nothing -> error "Unable to retrieve record"

adminGallery :: AppServer ()
adminGallery = do
  get (webRoute R.AdminCreateImage) $ do
    viewCreateImageAction
  post (webRoute R.AdminCreateImage) $ do
    createImageAction
  get (webRoute $ R.AdminDeleteImage "imageId") $ do
    param "imageId" >>= deleteImageAction

viewCreateImageAction :: AppAction ()
viewCreateImageAction = do
  let t = "Create Image"
  renderPage' t Nothing (panelWithErrorView t Nothing  $ Forms.Image.postFormLucid (Forms.File.postForm))

createImageAction :: AppAction ()
createImageAction = do
  let panel m v = panelWithErrorView "Create Image" (m) $ Forms.Image.postFormLucid v
  ct <- liftIO $ getCurrentTime
  initialInput <- scottyInput
  eid <- maybe "" (id) <$> (pure $ Map.lookup "fileTitle" $ Map.fromList initialInput)
  let f' =
          toList
        . Map.insert "fileId" ""
        . Map.insert "fileCreated" (show ct)
        . Map.adjust
            (  replace " " "-"
             . (\x -> bool x eid (x == ""))
            )
            "fileEasyId"
        . Map.fromList
  scottyInput >>= (form . f') >>= \case
    Right p -> do
      c <- liftAndCatchIO connection
      _ <- liftAndCatchIO $ ImageQueries.createImage p c
      redirect $ cs $ R.renderPublicUrl R.ListImage
    Left nferr -> do
      let extra = panel Nothing $ nferr
      liftIO $ pPrint nferr
      renderPage' ("Create Image") (Just ("Error submitting comment", NotificationError)) (extra)

form :: FormInput -> AppAction (Either (NioForm MNE.MyNioFieldError) Image)
form = runInputForm' Forms.File.postForm inputImage 

editImageAction :: Int -> AppAction ()
editImageAction x = do
  let panel m v = panelWithErrorView "Edit Image" (m) $ Forms.Image.postEditFormLucid x v
  ct <- liftIO $ getCurrentTime
  let f' =
          toList
        . Map.insert "imageCreated" (show ct)
        . Map.fromList
  scottyFormInput >>= form . f' >>= \case
    Right p -> AppAdminGalleryImage.processImage p ((flip panel) Forms.File.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      renderPage' ("Edit Image") (Just ("Error submitting comment", NotificationError)) (extra)

editImageAction' :: Int -> AppAction ()
editImageAction' x = do
  p <- getImageElseError x
  renderPage'
    "Edit Image"
    Nothing
    (
      panelWithErrorView "Edit Image" Nothing
      $ Forms.Image.postEditFormLucid
      (Image.imageId p)
      (postForm' $ imageToFile p)
    )

deleteImageAction :: Int -> AppAction ()
deleteImageAction x = do
  c <- liftAndCatchIO connection
  _ <- liftAndCatchIO $ ImageQueries.deleteImage x c
  redirect $ cs $ R.renderPublicUrl R.ListImage

processImage :: Image -> (Maybe Text -> Html ()) -> AppAction ()
processImage p _panel = do
  c <- liftAndCatchIO connection
  _ <- liftAndCatchIO $ ImageQueries.updateImage p c
  redirect $ cs $ R.renderPublicUrl R.ListImage
