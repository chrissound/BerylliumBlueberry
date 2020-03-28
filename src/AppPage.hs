{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
module AppPage where

import Control.Monad

import AppCommon
import App404

import Data.String
import Models.PostType
import Models.Post
import Template.Base
import AppRender
import Server
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Network.Wai
import Routes
import Data.Map
import Network.HTTP.Types

appPageServer :: AppServer ()
appPageServer = do
  notFound $ do
    status status200
    pp <- liftIO $ getPagePosts
    req <- rawPathInfo <$> request
    liftIO $ print req
    let ppp = Data.Map.fromList
          $
          fmap
          (\p -> (,)
            (cs $ "/" <> postEasyId p)
            (withSvRenderPage (postTitle p) (\sv -> pageView sv p))
          )
          (postV <$> pp)
    case Data.Map.lookup (req) ppp of
      Just x -> x
      Nothing -> app404Redirect

