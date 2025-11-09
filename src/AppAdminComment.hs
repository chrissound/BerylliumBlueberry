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
    verifyAuth
    commentListAction
  getPost (webRoute $ R.AdminApproveComment "commentId") $ do
    verifyAuth
    param "commentId" >>= approveCommentAction
  getPost (webRoute $ R.AdminDeleteComment "commentId") $ do
    verifyAuth
    param "commentId" >>= deleteCommentAction

commentListAction :: AppAction ()
commentListAction = do
  c <- liftAndCatchIO connection
  let sql = [NI.text|
      SELECT c."commentId", c."postId", c."commentBody", c."authorAlias", c."approved", c."postCreated", p."postTitle"
      FROM "comment" c
      JOIN "post" p ON c."postId" = p."postId"
      ORDER BY c."postCreated" DESC
      |]
  results <- liftAndCatchIO $ (query_ c (fromString $ cs sql) :: IO [(Int, Int, Text, Text, Bool, UTCTimestamp, Text)])
  let comments = map (\(cid, pid, cbody, author, appr, created, ptitle) ->
        CommentWithPost
          (Comment cid pid cbody author appr created)
          ptitle
        ) results
  withSvRenderPage "Comments" (\sv -> commentsView sv comments commentViewExtraAdmin)

approveCommentAction :: Int -> AppAction ()
approveCommentAction cid = do
  c <- liftAndCatchIO connection
  let sql = [NI.text|
      UPDATE "comment"
      SET "approved" = true
      WHERE "commentId" = ?
      |]
  _ <- liftAndCatchIO $ execute c (fromString $ cs sql) (Only cid)
  redirect $ cs $ R.renderPublicUrl R.AdminListComment

deleteCommentAction :: Int -> AppAction ()
deleteCommentAction cid = do
  c <- liftAndCatchIO connection
  let sql = [NI.text|
      DELETE FROM "comment"
      WHERE "commentId" = ?
      |]
  _ <- liftAndCatchIO $ execute c (fromString $ cs sql) (Only cid)
  redirect $ cs $ R.renderPublicUrl R.AdminListComment
