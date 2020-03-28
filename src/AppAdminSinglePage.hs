{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}

module AppAdminSinglePage where
import Data.Map as Map
import AppRender
import AppCommon
import qualified Routes as R
import Blog

import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM.Model

import Models.Post
import Models.PostType

import Forms.Post2

import Template.Base

import DatastoreHelper

import Data.Maybe
import Control.Monad.IO.Class
import Data.Text
import Lucid
import AppPostTypeI

adminSinglePage :: AppServer ()
adminSinglePage = do
  get (webRoute R.AdminListSinglePage) $ do
    verifyAuth
    singlePageList
  get (webRoute R.AdminCreateSinglePage) $ do
    verifyAuth
    let t = "Create Page"
    renderPage' t Nothing (panelWithErrorView t Nothing  $ Forms.Post2.pageFormLucid (Forms.Post2.postForm))
  post (webRoute R.AdminCreateSinglePage) $ do
    verifyAuth
    createPageAction
  getPost (webRoute $ R.AdminDeleteSinglePage "pageId") $ do
   verifyAuth
   param "pageId" >>= deletePostTypeActionByPostId
   param "pageId" >>= deletePostAction'

deletePostAction' :: Int -> AppAction ()
deletePostAction' x = do
  c <- liftAndCatchIO connection
  p <- liftAndCatchIO $ findRow c (DBRef $ fromIntegral x)
  case p of
    Just p' -> do
      d <- liftAndCatchIO $ destroy c (p' :: Post)
      case d of
        Right _ -> redirect $ cs $ R.renderPublicUrl R.AdminListSinglePage
        Left e -> error $ "Validation error:" ++ show e
    Nothing -> error $ "Post not found: " ++ show p

singlePageList :: AppAction ()
singlePageList = do
  c <- liftAndCatchIO connection
  postTypes <- liftAndCatchIO $ (dbSelect c $ addWhere "\"postType\" = ?" (Only $ fromEnum PostTypePage) (modelDBSelect :: DBSelect PostType))
  posts <- liftAndCatchIO $ (dbSelect c (modelDBSelect :: DBSelect Post))

  let m = catMaybes $ combineByIntIndex
            PagePost
            (idIntegerRef . Models.PostType.postId)
            (idInteger . Models.Post.postId)
            postTypes
            posts
  withSvRenderPage "Pages"  (\sv -> pageViewExtraAdmin' <> pagesView sv m pageViewExtraAdmin)

createPageAction :: AppAction ()
createPageAction = do
  let panel m v = panelWithErrorView "Create Page" (m) $ Forms.Post2.pageFormLucid v
  ct <- liftIO $ getCurrentTime
  initialInput <- scottyInput
  let formInput =
          Map.insert "approved" "0"
        $ Map.insert "postCreated" (show ct)
        $ Map.fromList initialInput
  let f = myRunForm Forms.Post2.postForm inputPost $ toList formInput
  case f of
    Right p -> processPost p ((flip panel) Forms.Post2.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      renderPage' ("Create Page") (Just ("Error submitting comment", NotificationError)) (extra)

processPost :: Post -> (Maybe Text -> Html ()) -> AppAction ()
processPost p panel = do
  c <- liftAndCatchIO connection
  (liftAndCatchIO $ trySave c p) >>= \case
    Right p' -> do
      (
        liftAndCatchIO $
        trySave c (PostType NullKey (DBRef $ fromIntegral . idInteger $ Models.Post.postId p') (fromEnum PostTypePage))
        ) >>= \case
        Right _ -> do
          redirect $ cs $ R.renderPublicUrl R.ListPost
        Left e -> renderScottyHtml $ panel (Just $ cs $ show e)
    Left e -> renderScottyHtml $ panel (Just $ cs $ show e)
