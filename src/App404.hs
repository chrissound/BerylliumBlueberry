{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
module App404 where

import Control.Monad

import AppCommon
import AppConfig
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

app404Redirect :: AppAction ()
app404Redirect = do
  mapping <- liftIO $ siteRedirect <$> getAppConfig
  req <- rawPathInfo <$> request
  liftIO $ print req
  let ppp = Data.Map.fromList (fmap (\(a,b) -> (cs a, cs b))  mapping)
  case Data.Map.lookup (req) ppp of
    Just x -> do
      status status301
      setHeader "Location" $ x
    Nothing -> case req of
      "/" -> do
        status status301
        setHeader "Location" $ cs $ renderPublicUrl ListPost
      _ -> status status404
