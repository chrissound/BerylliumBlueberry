{-# OPTIONS -Wno-unused-imports #-}
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
import Database.PostgreSQL.ORM.Model
import Template.Base

import ScottyInput

import AppRender
import AppCommon
import AppPost
import AdminSettings

import Database.PostgreSQL.Simple
import Database.PostgreSQL.ORM
import Data.String


getPostTypeWherePostId :: Int -> AppAction PostType
getPostTypeWherePostId x = do
  c <- liftAndCatchIO connection
  p <- liftAndCatchIO $
    dbSelect c $
      (addWhere (fromString $ cs $ sqlWhereEquals "postId") (Only x) modelDBSelect )
  case p of
    [p'] -> pure p'
    _ -> error $ "PostType not found: " ++ show p

deletePostTypeActionByPostId :: Int -> AppAction ()
deletePostTypeActionByPostId x = do
  c <- liftAndCatchIO connection
  p <- liftAndCatchIO $ findRow c (DBRef $ fromIntegral x)
  case p of
    Just p' -> do
      pt <- getPostTypeWherePostId (idInteger $ Post.postId p')
      deletePostTypeAction pt
    Nothing -> error $ "Post not found: " ++ show p


deletePostTypeAction :: PostType -> AppAction ()
deletePostTypeAction p = do
  c <- liftAndCatchIO connection
  d <- liftAndCatchIO $ destroy c (p :: PostType)
  case d of
    Right _ -> pure ()
    Left e -> error $ "Validation error:" ++ show e
