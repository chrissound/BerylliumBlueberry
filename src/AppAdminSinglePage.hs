{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wno-unused-imports #-}

module AppAdminSinglePage where
import Data.Map as Map
import AppRender
import AppCommon
import qualified Routes as R
import Blog

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI

import Models.Post
import Models.PostType
import Models.PostQueries as PostQueries
import Models.PostTypeQueries as PostTypeQueries

import Forms.Post2

import Template.Base

import DatastoreHelper

import Data.Maybe
import Control.Monad.IO.Class
import Data.Text
import Lucid
import AppPostTypeI
import NioForm

adminSinglePage :: AppServer ()
adminSinglePage = do
  get (webRoute R.AdminListSinglePage) $ do
    singlePageList
  get (webRoute R.AdminCreateSinglePage) $ do
    let t = "Create Page"
    sv <- (svd t Nothing)
    let sv' =
          sv
            { scripts =
                   [ ( "//code.jquery.com/jquery-2.2.4.min.js"
                     , Just "sha256-BbhdlvQf/xTY9gja0Dq3HiwQF8LaCRTXxZKRutelT44"
                     , Just "anonymous"
                     )
                   , ( "//cdn.jsdelivr.net/npm/select2@4.1.0-beta.1/dist/js/select2.min.js"
                     , Nothing
                     , Nothing
                     )
                   , ("/static/postform.js", Nothing, Nothing)
                   ]
            , css     =
                [ "//cdn.jsdelivr.net/npm/select2@4.1.0-beta.1/dist/css/select2.min.css"
                   ]
            }
    renderScottyHtmlSv sv' (panelWithErrorView t Nothing  $ Forms.Post2.pageFormLucid (Forms.Post2.postForm))
  post (webRoute R.AdminCreateSinglePage) $ do
    createPageAction
  getPost (webRoute $ R.AdminDeleteSinglePage "pageId") $ do
   param "pageId" >>= deletePostTypeActionByPostId
   param "pageId" >>= deletePostAction'

deletePostAction' :: Int -> AppAction ()
deletePostAction' x = do
  c <- liftAndCatchIO connection
  mPost <- liftAndCatchIO $ PostQueries.getPostById x c
  case mPost of
    Just _ -> do
      _ <- liftAndCatchIO $ PostQueries.deletePost x c
      redirect $ cs $ R.renderPublicUrl R.AdminListSinglePage
    Nothing -> error $ "Post not found"

singlePageList :: AppAction ()
singlePageList = do
  c <- liftAndCatchIO connection
  postTypes <- liftAndCatchIO $ PostTypeQueries.getPostTypesByType PostTypePage c
  posts <- liftAndCatchIO $ PostQueries.getAllPosts c

  let m = catMaybes $ combineByIntIndex
            PagePost
            (Models.PostType.postId)
            (Models.Post.postId)
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
  let f = runInputForm Forms.Post2.postForm inputPost $ toList formInput
  case f of
    Right p -> processPost p ((flip panel) Forms.Post2.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      renderPage' ("Create Page") (Just ("Error submitting comment", NotificationError)) (extra)

processPost :: Post -> (Maybe Text -> Html ()) -> AppAction ()
processPost p _panel = do
  c <- liftAndCatchIO connection
  insertedPostId <- liftAndCatchIO $ PostQueries.createPost p c
  let pt = PostType 0 insertedPostId (fromEnum PostTypePage :: Int)
  _ <- liftAndCatchIO $ PostTypeQueries.createPostType pt c
  redirect $ cs $ R.renderPublicUrl R.ListPost
