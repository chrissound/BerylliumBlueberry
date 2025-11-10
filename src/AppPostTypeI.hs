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
import Models.PostQueries as PostQueries
import Models.PostTypeQueries as PostTypeQueries
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
  mPostType <- liftAndCatchIO $ PostTypeQueries.getPostTypeByPostId x c
  case mPostType of
    Just pt -> pure pt
    Nothing -> error $ "PostType not found for postId: " ++ show x

deletePostTypeActionByPostId :: Int -> AppAction ()
deletePostTypeActionByPostId x = do
  c <- liftAndCatchIO connection
  mPost <- liftAndCatchIO $ PostQueries.getPostById x c
  case mPost of
    Just p' -> do
      pt <- getPostTypeWherePostId (Post.postId p')
      deletePostTypeAction pt
    Nothing -> error $ "Post not found"


deletePostTypeAction :: PostType -> AppAction ()
deletePostTypeAction p = do
  c <- liftAndCatchIO connection
  _ <- liftAndCatchIO $ PostTypeQueries.deletePostTypeById (PostType.postTypeId p) c
  pure ()
