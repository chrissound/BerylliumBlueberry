{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}

module AdminSettings where

import AppRender
import AppCommon
import qualified Routes as R
import Blog
import Template.Base
import AppConfig
import Control.Monad.Trans
import Data.Map as Map
import Data.Aeson

import Forms.AdminSettings

adminSettings :: AppServer ()
adminSettings = do
  get (webRoute R.AdminSettings) $ do
    verifyAuth
    appConfig <- liftIO getAppConfig
    let t = "Settings"
    renderPage' t
      Nothing
      (panelWithErrorView t Nothing  $ Forms.AdminSettings.postFormLucid (Forms.AdminSettings.postForm $ Just appConfig))
  post (webRoute R.AdminSettings) $ do
    verifyAuth
    let t = "Settings"
    formInput <- scottyInput
    case ((myRunForm (Forms.AdminSettings.postForm Nothing) inputPost) formInput) of
      Right conf -> do
        liftIO $ encodeFile "data/config.json" (conf  :: AppConfig)
        liftIO $ encodeFile "data/configxxx.json" (conf  :: AppConfig)
        renderPage' t
          (Just ("Saved", NotificationInfo))
          (panelWithErrorView t Nothing $ Forms.AdminSettings.postFormLucid (Forms.AdminSettings.postForm $ Just conf))
      Left nferr -> do
        liftIO $ pPrint nferr
        liftIO $ pPrint $ inputPost formInput
        renderPage' t
          (Just ("Error", NotificationError))
          (panelWithErrorView t Nothing  $ Forms.AdminSettings.postFormLucid nferr)
