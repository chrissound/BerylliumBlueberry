{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# Options -Wno-unused-matches #-}

module App where

import Web.Scotty.Trans
import Network.Wai.Middleware.Static
import Network.Wai
import Control.Concurrent.STM (TVar)

import AppAdmin
import AppCommon
import AppComment
import AppPost
import AppPage
import AppImage
import AppFile
import Server (AppState)
import Middleware.AdminAuth (adminAuthMiddleware)
import AppConfig


guestServer :: AppServer ()
guestServer = do
  -- get "/api/debug" $ do
  --   appIO $ print "fuck"
  --   lift getAppState >>= appIO . print
  --   pure ()
  middleware $ staticPolicy (hasPrefix "static/" <> noDots <> addBase ".")
  middleware $ staticPolicy (hasPrefix "files/" <> noDots <> addBase ".")
  middleware $ staticPolicy (hasPrefix "data/uploads" <> noDots <> addBase ".")
  appComment
  listPost
  listImage
  listFile
  appPageServer

server :: AppConfig -> Middleware -> AppServer ()
server appConfig logger = do
  middleware logger
  middleware $ adminAuthMiddleware appConfig
  adminServer
  guestServer
