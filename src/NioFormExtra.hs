{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-unused-imports #-}

module NioFormExtra where

import NioForm
import NioFormM
import NioFormTypes
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model
import Web.Scotty.Trans
import Models.Post
import AppTypes
import qualified Data.Text.Lazy as L
import Data.String.Conversions
import Text.Read (readMaybe)
import Control.Monad.IO.Class
import Data.List (find)
import Network.Wai.Parse
import Data.ByteString.Lazy as BSL (writeFile)
import Models.Image
import Models.File
import System.Process.Typed
import System.Exit
import Data.String
import Data.Word

instance FieldGetterM (AppActionT) (ImageResizedFileUpload) where
  getFieldM x = do
    x' <- files
    case find ((== (cs x)) . fst ) (x') of
      Just (_, FileInfo fn _ c) -> do
        case (c) of
          "" -> pure $ Left "No file uploaded"
          _ -> do
            p <- imageDataPath
            pt' <- imageThumbDataPath
            let fullImagePath = p ++"/"++ cs fn
            let fullImageThumbPath' = pt' ++"/"++ cs fn
            liftIO $ do
              BSL.writeFile fullImagePath c
              BSL.writeFile fullImageThumbPath' c
            (runProcess $ proc "convert" [fullImageThumbPath', "-scale", "250x250"]) >>= \case
              ExitSuccess -> pure $ Right ImageResizedFileUpload
              e -> pure $ Left $ "Unable to resize image: " ++ show e
      Nothing -> do
        pure $ Left "No file uploaded"

instance FieldGetterM (AppActionT) (FileUpload) where
  getFieldM x = do
    x' <- files
    case find ((== (cs x)) . fst ) (x') of
      Just (_, FileInfo fn _ c) -> do
        case (c) of
          "" -> pure $ Left "No file uploaded"
          _ -> do
            p <- fileDataPath
            let fullImagePath = p ++"/"++ cs fn
            liftIO $ print fullImagePath
            liftIO $ BSL.writeFile fullImagePath c
            pure $ Right FileUpload
      Nothing -> do
        pure $ Left "No file uploaded"

instance FieldGetterM (AppActionT) (FileUploadName) where
  getFieldM x = do
    x' <- files
    case find ((== (cs x)) . fst ) (x') of
      Just (_, FileInfo fn _ _) -> do
        pure $ Right $ FileUploadName $ cs fn
      Nothing -> do
        pure $ Left "No file uploaded"

instance FieldGetter UTCTimestamp where
  getField x = case readMaybe x of
      Just x' -> Right x'
      Nothing -> do
        Left $ "Invalid UTCTimestamp for: " <> x

instance FieldGetter Word16 where
  getField x = case readMaybe x of
      Just x' -> Right (fromIntegral x')
      Nothing -> do
        Left $ "Invalid integer: " <> x

instance FieldGetter DBKey where
  getField x = case x of
    "" -> Right NullKey
    _ -> do
      case readMaybe x of
        Just x' -> Right $ DBKey x'
        Nothing -> Left $ "error DBKey for: " <> x

instance FieldGetter (DBRef a) where
  getField x = case readMaybe x of
        Just x' -> Right $ DBRef x'
        Nothing -> Left $ "error DBRef for: " <> x

instance FieldGetter Post where
  getField _ = undefined
