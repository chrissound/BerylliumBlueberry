{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module AppPost where

import AppCommon
import AppRender

import Models.Post as Post
import Models.PostType

import qualified Routes as R
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model
import Template.Base
import Template.Post
import Forms.Comment
import Models.Associations
import Lucid
import Data.Text (Text)
import Data.List
import Network.HTTP.Types

getPostElseError :: Int -> AppAction Post.Post
getPostElseError x = do
  c <- liftAndCatchIO connection
  u <- liftAndCatchIO $ findRow c (DBRef $ fromIntegral x)
  case u of
    Just u' -> return u'
    Nothing -> error "Unable to retrieve record"


postsViewPage :: AppAction ()
postsViewPage = do
  posts <- liftAndCatchIO $ getPagePosts' (PostTypeBlog)
  (postHook, preHook) <- AppCommon.session >>= \case
    Just _ -> return (postViewExtraAdmin, postsViewExtraAdmin)
    Nothing -> return (postViewExtraEmpty, return ())
  withSvRenderPage "Blog" (\sv -> preHook <> postsView sv (postV <$> posts) postHook "Blog")

postsViewPageTag :: Text -> AppAction ()
postsViewPageTag x = do
  posts <- fmap (filter (elem x . getPostTags . postTags . postV)) $ liftAndCatchIO $ getPagePosts' (PostTypeBlog)
  case posts of
    [] -> status status404
    _ -> do
      (postHook, preHook) <- AppCommon.session >>= \case
        Just _ -> return (postViewExtraAdmin, postsViewExtraAdmin)
        Nothing -> return (postViewExtraEmpty, return ())
      let t = mconcat $ intersperse " " ["Blog","-",x]
      withSvRenderPage t (\sv -> preHook <> postsView sv (postV <$> posts) postHook (cs t))


listPost :: AppServer ()
listPost = do
  get (webRoute R.ListPost)$ do
    postsViewPage
  get (webRoute $ R.TaggedPost "tag") $ do
    t <- param "tag"
    postsViewPageTag t
  get (webRoute $ R.ViewPost "postId")$ do
    c <- liftAndCatchIO connection
    (param "postId" >>= return . filterEid)
      >>= maybe404 (
      \eid' -> 

        (liftAndCatchIO $ getPostByEasyId eid' c)
        >>=
          maybe404
            (\p -> do
              comments <- liftIO $ dbSelect c $ approvedPostComment p
              extra <- pure $ do
                panel' "Comments" $ if (Prelude.length comments > 0) then
                  mconcat $ commentView <$> comments
                  else
                    p_ "No comments, yet!"
                panelWithErrorView "Submit a comment" Nothing $ commentFormLucid $ commentForm' p
              withSvRenderPage (postTitle p) (\sv -> postView sv p extra)
            )
      )
