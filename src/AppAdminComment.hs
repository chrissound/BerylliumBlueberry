{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -Wno-unused-imports #-}

module AppAdminComment where

import qualified Routes as R
import Blog
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI
import Models.Comment
import Models.Post
import Models.CommentQueries as CommentQueries
import AppCommon
import AppRender
import Template.Base
import Template.Comment (CommentWithPost(..), commentsView, commentViewExtraAdmin)
import Lucid
import Data.Maybe
import Control.Monad.IO.Class
import Data.Text (Text)

adminComment :: AppServer ()
adminComment = do
  get (webRoute R.AdminListComment) $ do
    commentListAction
  getPost (webRoute $ R.AdminApproveComment "commentId") $ do
    param "commentId" >>= approveCommentAction
  getPost (webRoute $ R.AdminDeleteComment "commentId") $ do
    param "commentId" >>= deleteCommentAction

commentListAction :: AppAction ()
commentListAction = do
  c <- liftAndCatchIO connection
  results <- liftAndCatchIO $ CommentQueries.listAllCommentsWithPostTitles c
  let comments = map (\(cid, pid, cbody, author, appr, created, ptitle) ->
        CommentWithPost
          (Comment cid pid cbody author appr created)
          ptitle
        ) results
  withSvRenderPage "Comments" (\sv -> commentsView sv comments commentViewExtraAdmin)

approveCommentAction :: Int -> AppAction ()
approveCommentAction cid = do
  c <- liftAndCatchIO connection
  _ <- liftAndCatchIO $ CommentQueries.approveComment cid c
  redirect $ cs $ R.renderPublicUrl R.AdminListComment

deleteCommentAction :: Int -> AppAction ()
deleteCommentAction cid = do
  c <- liftAndCatchIO connection
  _ <- liftAndCatchIO $ CommentQueries.deleteComment cid c
  redirect $ cs $ R.renderPublicUrl R.AdminListComment
