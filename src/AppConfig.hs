{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-orphans #-}
module AppConfig where

import GHC.Generics
import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple

data AppConfig = AppConfig {
    siteHeading :: Text
  , siteSubHeading :: Text
  , siteRedirect :: [(Text,Text)]
  , siteContactEmail :: Text
  , siteSideBarHtml :: Text
  , siteExtraHeadHtml :: Text
  , databaseConnection :: ConnectInfo
} deriving (Generic, Show)

data AppConfigFields =
    AppConfigSiteHeading
  | AppConfigSiteSubHeading
  | AppConfigSiteRedirect
  | AppConfigSiteContactEmail
  | AppConfigSiteSideBarHtml
  | AppConfigSiteExtraHeadHtml
  | AppConfigDatabaseConnectionHost
  | AppConfigDatabaseConnectionUser
  | AppConfigDatabaseConnectionPassword
  | AppConfigDatabaseConnectionName
  | AppConfigDatabaseConnectionPort
  deriving (Show, Enum)

instance ToJSON AppConfig where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON AppConfig

instance ToJSON ConnectInfo where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ConnectInfo


getAppConfig :: IO AppConfig
getAppConfig = do
  encodeFile "data/config.sample.json" $
    AppConfig "" "" ([("/test","/abc")]) "" "" "" (ConnectInfo "" 3000 "" "" "")
  eitherDecodeFileStrict "data/config.json" >>= \case
    Right x -> pure x
    Left e -> error $ "App config failed to load: " <> show e


