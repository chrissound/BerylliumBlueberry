{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE QuasiQuotes #-}
module AppPostTypeI where

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
import Data.List.Utils (replace)

import Lucid

import Forms.Login
-- import Forms.Post
import Forms.Post2
import NioForm
import NioFormExtra

import Models.Post as Post
import Models.PostType as PostType
import Models.User as M
import User
import Database

import Data.ByteString.Base64 as B64
import Template.Base

import ScottyInput

import AppRender
import AppCommon
import AppPost
import AdminSettings

import Database.PostgreSQL.Simple
import Data.String
import Data.String.Conversions
import qualified NeatInterpolation as NI


getPostTypeWherePostId :: Int -> AppAction PostType
getPostTypeWherePostId x = do
  c <- liftAndCatchIO connection
  let sql = [NI.text|
      SELECT "postTypeId", "postId", "postType"
      FROM "postType"
      WHERE "postId" = ?
      |]
  p <- liftAndCatchIO $ (query c (fromString $ cs sql) (Only x) :: IO [PostType])
  case p of
    [p'] -> pure p'
    _ -> error $ "PostType not found: " ++ show p

deletePostTypeActionByPostId :: Int -> AppAction ()
deletePostTypeActionByPostId x = do
  c <- liftAndCatchIO connection
  let sqlPost = [NI.text|
      SELECT "postId", "postTitle", "postBody", "postCreated", "postEasyId", "postTags"
      FROM "post"
      WHERE "postId" = ?
      |]
  p <- liftAndCatchIO $ (query c (fromString $ cs sqlPost) (Only x) :: IO [Post.Post])
  case p of
    (p':_) -> do
      pt <- getPostTypeWherePostId (Post.postId p')
      deletePostTypeAction pt
    [] -> error $ "Post not found"


deletePostTypeAction :: PostType -> AppAction ()
deletePostTypeAction p = do
  c <- liftAndCatchIO connection
  let sql = [NI.text|
      DELETE FROM "postType"
      WHERE "postTypeId" = ?
      |]
  _ <- liftAndCatchIO $ execute c (fromString $ cs sql) (Only $ PostType.postTypeId p)
  pure ()
