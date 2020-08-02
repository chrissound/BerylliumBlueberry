{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}

module AppAdminFile where 

import Data.Map as Map
import qualified Routes as R
import Web.Cookie
import Web.Scotty.Cookie
import System.Entropy
import Control.Monad.Reader
import Blog

import Data.Time.Clock
import Data.Text (Text, map)
import Data.Bool

import Lucid

import Forms.File
import NioForm
import NioFormExtra
import NioFormTypes

import Models.File as File
import User
import Database

import Data.ByteString.Base64 as B64
import Database.PostgreSQL.ORM.Model
import Template.Base

import ScottyInput

import AppRender
import AppCommon hiding (File, files)
import AppPost
import AppAdminSinglePage
import AdminSettings
import Text.Pretty.Simple

import Data.List.Utils (replace)

getFileElseError :: Int -> AppAction File
getFileElseError x = do
  c <- liftAndCatchIO connection
  u <- liftAndCatchIO $ findRow c (DBRef $ fromIntegral x)
  case u of
    Just u' -> return u'
    Nothing -> error "Unable to retrieve record"

adminFile :: AppServer ()
adminFile = do
  get (webRoute R.AdminCreateFile) $ do
    verifyAuth
    viewCreateFileAction
  post (webRoute R.AdminCreateFile) $ do
    verifyAuth
    createFileAction
  get (webRoute $ R.AdminDeleteFile "fileId") $ do
    verifyAuth
    param "fileId" >>= deleteFileAction

viewCreateFileAction :: AppAction ()
viewCreateFileAction = do
  let t = "Create File"
  renderPage' t Nothing (panelWithErrorView t Nothing  $ Forms.File.postFormLucid (Forms.File.postForm))

createFileAction :: AppAction ()
createFileAction = do
  let panel m v = panelWithErrorView "Create File" (m) $ Forms.File.postFormLucid v
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
      (liftAndCatchIO $ trySave c (p)) >>= \case
        Right _ -> redirect $ cs $ R.renderPublicUrl R.ListFile
        Left e -> renderScottyHtml $ panel (Just $ cs $ show e) (Forms.File.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      liftIO $ pPrint nferr
      renderPage' ("Create File") (Just ("Error submitting comment", NotificationError)) (extra)

form :: FormInput -> AppAction (Either NioForm File)
form = runInputForm' Forms.File.postForm inputFile 

editFileAction :: Int -> AppAction ()
editFileAction x = do
  let panel m v = panelWithErrorView "Edit File" (m) $ Forms.File.postEditFormLucid x v
  ct <- liftIO $ getCurrentTime
  let f' =
          toList
        . Map.insert "fileCreated" (show ct)
        . Map.fromList
  scottyFormInput >>= form . f' >>= \case
    Right p -> AppAdminFile.processFile p ((flip panel) Forms.File.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      renderPage' ("Edit File") (Just ("Error submitting comment", NotificationError)) (extra)

editFileAction' :: Int -> AppAction ()
editFileAction' x = do
  p <- getFileElseError x
  renderPage'
    "Edit File"
    Nothing
    (
      panelWithErrorView "Edit File" Nothing
      $ Forms.File.postEditFormLucid
      (idInteger $ File.fileId p)
      (postForm' p)
    )

deleteFileAction :: Int -> AppAction ()
deleteFileAction x = do
  c <- liftAndCatchIO connection
  p <- liftAndCatchIO $ findRow c (DBRef $ fromIntegral x)
  case p of
    Just p' -> do
      d <- liftAndCatchIO $ destroy c (p' :: File)
      case d of
        Right _ -> redirect $ cs $ R.renderPublicUrl R.ListFile
        Left e -> error $ "Validation error:" ++ show e
    Nothing -> error $ "File not found: " ++ show p

processFile :: File -> (Maybe Text -> Html ()) -> AppAction ()
processFile p panel = do
  c <- liftAndCatchIO connection
  (liftAndCatchIO $ trySave c (p)) >>= \case
    Right _ -> redirect $ cs $ R.renderPublicUrl R.ListFile
    Left e -> renderScottyHtml $ panel (Just $ cs $ show e)
