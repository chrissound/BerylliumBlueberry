{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
module AppComment where

import Database.PostgreSQL.Simple
import Data.Map as Map
import qualified Routes as R
import Control.Monad.IO.Class
import Data.String.Conversions
import Data.Text
import Lucid

import Template.Base
import Template.Post
import Forms.Comment
--import Models.Comment as M
import Models.Comment as Comment
import Models.Post as Post
import AppCommon
import AppRender
import NioFormHtml
import NioForm

import Models.Associations
-- import qualified Data.Map as Map
import Text.Read (readMaybe)
import qualified NeatInterpolation as NI
import Data.String

appComment :: AppServer ()
appComment = do
  post (webRoute $ R.CreateCommentOnPost) $ do
    ct <- liftIO $ getCurrentTime
    formInput <- fmap (\(a,b) -> (cs a :: String, cs b :: String)) <$> params

    let entityText = "comment"
    let fullInput =
            Map.insert "approved" "0"
          $ Map.insert "postCreated" (show ct)
          $ Map.fromList formInput
    let procForm = runInputForm commentForm inputComment $ Map.toList fullInput
    let panel m = panelWithErrorView ("Create " <> entityText) m $ commentFormLucid commentForm
    case procForm of
      Right commentRecord -> processComment commentRecord panel
      Left nferr -> do
        liftIO $ pPrint nferr
        case (Map.lookup "postId" (Map.fromList formInput) >>= readMaybe) of
          Nothing -> error "something went wrong??"
          Just postIdInput -> do
            c <- liftIO $ connection
            let sqlPost = [NI.text|
                SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
                FROM "post"
                WHERE "postId" = ?
                |]
            postResults <- liftIO (query c (fromString $ cs sqlPost) (Only (postIdInput :: Int)) :: IO [Post])
            p <- case postResults of
              (x:_) -> pure x
              [] -> error "Post not found"
            let sqlComments = [NI.text|
                SELECT "commentId", "postId", "commentBody", "authorAlias", "approved", "commentCreated"
                FROM "comment"
                WHERE "postId" = ? AND "approved" = true
                |]
            comments <- liftIO $ query c (fromString $ cs sqlComments) (Only $ Post.postId p)
            extra <- pure $ do
              panel' "Comments" $ if (Prelude.length comments > 0) then
                mconcat $ commentView <$> comments
                else
                  p_ "No comments !"
              panelWithErrorView "Submit a comment" Nothing $ commentFormLucid nferr
            withSvRenderPage'
              (postTitle p)
              (Just ("Form error, invalid input.", NotificationError))
              (\sv -> postView sv p extra)

processComment :: Comment -> (Maybe Text -> Html ()) -> AppAction ()
processComment r _panel = do
  c <- liftAndCatchIO connection
  let sqlInsert = [NI.text|
      INSERT INTO "comment" ("commentId", "postId", "commentBody", "authorAlias", "approved", "commentCreated")
      VALUES (?, ?, ?, ?, ?, ?)
      |]
  liftAndCatchIO (execute c (fromString $ cs sqlInsert) r) >>= \_ -> do
    (p, extra) <- liftIO $ do
      c' <- connection
      let sqlPost = [NI.text|
          SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
          FROM "post"
          WHERE "postId" = ?
          |]
      postResults <- (query c' (fromString $ cs sqlPost) (Only $ Comment.postId r) :: IO [Post])
      p <- case postResults of
        (x:_) -> pure x
        [] -> error "Post not found"
      let sqlComments = [NI.text|
          SELECT "commentId", "postId", "commentBody", "authorAlias", "approved", "commentCreated"
          FROM "comment"
          WHERE "postId" = ? AND "approved" = true
          |]
      comments <- query c' (fromString $ cs sqlComments) (Only $ Post.postId p)
      extra <- pure $ do
        panel' "Comments" $ mconcat $ commentView <$> comments
        panelWithErrorView "Submit a comment" Nothing $ commentFormLucid $ commentForm' p
      pure (p, extra)
    withSvRenderPage'
      (postTitle p)
      (Just ("Comment submitted", NotificationInfo))
      (\sv -> postView sv p extra)

