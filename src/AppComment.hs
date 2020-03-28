{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module AppComment where

import Database.PostgreSQL.ORM
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

import Models.Associations
-- import qualified Data.Map as Map
import Text.Read.Extra

appComment :: AppServer ()
appComment = do
  post (webRoute $ R.CreateCommentOnPost) $ do
    ct <- liftIO $ getCurrentTime
    let entityText = "comment"
    formInput <- fmap (\(a,b) -> (cs a :: String, cs b :: String)) <$> params
    let fullInput =
            Map.insert "approved" "0"
          $ Map.insert "postCreated" (show ct)
          $ Map.fromList formInput
    let procForm = mydbg "procForm"
          $ myRunForm commentForm (inputComment)
          $ mydbg "fullInput"
          $ Map.toList fullInput
    let panel m nf = panelWithErrorView ("Create " <> entityText) m $ commentFormLucid nf
    (pure $ procForm) >>= \case
      Right commentRecord ->
        processComment commentRecord ((flip panel) commentForm)
      Left nferr -> do
        case (Map.lookup "postId" (Map.fromList formInput) >>= readMaybe) of
          Nothing -> error "something went wrong??"
          Just  postIdInput -> do
            c <- liftIO $ connection
            p <- (liftIO $ findRow c $ postIdInput) >>= todoMaybeQuickError
            comments <- liftIO $ dbSelect c $ approvedPostComment p
            extra <- pure $ do
              panel' "Comments" $ if (Prelude.length comments > 0) then
                mconcat $ commentView <$> comments
                else
                  p_ "No comments !"
              panelWithErrorView "Submit a comment" Nothing $ commentFormLucid nferr
            withSvRenderPage'
              (postTitle p)
              (Just ("Form validation failed - invalid input.", NotificationError))
              (\sv -> postView sv p extra)

processComment :: Comment -> (Maybe Text -> Html ()) -> AppAction ()
processComment r panel = do
  liftAndCatchIO connection >>= (liftAndCatchIO . flip trySave r) >>= \case
    Right _ -> do
      c <- liftIO $ connection
      p <- (liftIO $ findRow c $ Comment.postId r) >>= todoMaybeQuickError
      comments <- liftIO $ dbSelect c $ approvedPostComment p
      extra <- pure $ do
        panel' "Comments" $ mconcat $ commentView <$> comments
        panelWithErrorView "Submit a comment" Nothing $ commentFormLucid $ commentForm' p
      withSvRenderPage'
        (postTitle p)
        (Just ("Comment submitted", NotificationInfo))
        (\sv -> postView sv p extra)
    Left e -> renderScottyHtml $ panel (Just $ cs $ show e)

