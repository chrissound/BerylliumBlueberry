{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# Options -Wno-unused-matches #-}

module App where

import Web.Scotty.Trans
import Network.Wai.Middleware.Static
import Network.Wai

import AppAdmin
import AppCommon
import AppComment
import AppPost
import AppPage
import AppImage
import AppFile


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

server :: Middleware -> AppServer ()
server logger = do
  middleware logger
  adminServer
  guestServer
