{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-orphans #-}
module AppConfig where

import GHC.Generics
import Data.Aeson
import Data.Text
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import System.Environment (lookupEnv)

-- Type for JSON file (without admin credentials)
data AppConfigJson = AppConfigJson {
    jsonSiteHeading :: Text
  , jsonSiteSubHeading :: Text
  , jsonSiteRedirect :: [(Text,Text)]
  , jsonSiteContactEmail :: Text
  , jsonSiteSideBarHtml :: Text
  , jsonSiteExtraHeadHtml :: Text
  , jsonDatabaseConnection :: ConnectInfo
} deriving (Generic, Show)

-- Full config with admin credentials from env vars
data AppConfig = AppConfig {
    siteHeading :: Text
  , siteSubHeading :: Text
  , siteRedirect :: [(Text,Text)]
  , siteContactEmail :: Text
  , siteSideBarHtml :: Text
  , siteExtraHeadHtml :: Text
  , databaseConnection :: ConnectInfo
  , adminUsername :: Text
  , adminPassword :: Text
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
    toEncoding (AppConfig h sh r e s eh db _ _) =
        toEncoding $ AppConfigJson h sh r e s eh db

instance FromJSON AppConfigJson where
    parseJSON = withObject "AppConfigJson" $ \v -> AppConfigJson
        <$> v .: "siteHeading"
        <*> v .: "siteSubHeading"
        <*> v .: "siteRedirect"
        <*> v .: "siteContactEmail"
        <*> v .: "siteSideBarHtml"
        <*> v .: "siteExtraHeadHtml"
        <*> v .: "databaseConnection"

instance ToJSON AppConfigJson where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON ConnectInfo where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ConnectInfo


getAppConfig :: IO AppConfig
getAppConfig = do
  configJson <- eitherDecodeFileStrict "data/config.json" >>= \case
    Right x -> pure x
    Left e -> error $ "App config failed to load: " <> show e

  -- Read admin credentials from environment variables
  adminUser <- lookupEnv "ADMIN_USERNAME" >>= \case
    Just u -> pure $ T.pack u
    Nothing -> error "ADMIN_USERNAME environment variable not set"

  adminPass <- lookupEnv "ADMIN_PASSWORD" >>= \case
    Just p -> pure $ T.pack p
    Nothing -> error "ADMIN_PASSWORD environment variable not set"

  -- Convert AppConfigJson to AppConfig with credentials from env
  pure $ AppConfig
    (jsonSiteHeading configJson)
    (jsonSiteSubHeading configJson)
    (jsonSiteRedirect configJson)
    (jsonSiteContactEmail configJson)
    (jsonSiteSideBarHtml configJson)
    (jsonSiteExtraHeadHtml configJson)
    (jsonDatabaseConnection configJson)
    adminUser
    adminPass


