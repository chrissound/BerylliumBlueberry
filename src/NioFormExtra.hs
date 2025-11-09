{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# OPTIONS -Wno-unused-imports #-}

module NioFormExtra where

import NioForm
import NioFormTypes
import NioFormInstances
import Database.PostgreSQL.Simple.Time
import Web.Scotty.Trans
import Models.Post
import AppTypes
--import qualified Data.Text.Lazy as L
import Data.Text (Text)
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
import Control.Concurrent
import Control.Monad.Identity
import Data.List
import Data.Tuple.Extra
import Common
import Debug.Trace
import qualified MyNioFieldError as MNE
import Data.Proxy

instance FieldGetterErrorKey AppActionT ImageResizedFileUpload NioFormKey where
  getFieldErrorKey k _ = pure k

instance FieldGetter AppActionT ImageResizedFileUpload MNE.MyNioFieldError NioFormKey where
  getField x i = do
    x' <- files
    case find ((== (cs x)) . fst ) (x') of
      Just (_, FileInfo fn _ c) -> do
        case (c) of
          "" -> pure $ Just $ Left (x, [(x, MNE.MyNioFieldErrorNotPresent)])
          _ -> do
            p <- imageDataPath
            pt' <- imageThumbDataPath
            let fullImagePath = p ++"/"++ cs fn
            let fullImageThumbPath' = pt' ++"/"++ cs fn
            liftIO $ do
              BSL.writeFile fullImagePath c
              BSL.writeFile fullImageThumbPath' c
            (runProcess $ proc "convert" [fullImageThumbPath', "-scale", "250x250>", fullImageThumbPath']) >>= \case
              ExitSuccess -> pure $ Just $ Right $ ImageResizedFileUpload $ cs fn
              e -> pure $ Just $ Left $ (x, [(x, MNE.MyNioFailedValidation $ "Unable to resize image: " ++ show e)])
      Nothing -> do
        pure $ Just $ Left (x, [(x, MNE.MyNioFieldErrorNotPresent)])

instance FieldGetterErrorKey AppActionT FileUpload NioFormKey where
  getFieldErrorKey k _ = pure k

instance FieldGetter (AppActionT) (FileUpload) MNE.MyNioFieldError NioFormKey where
  getField x i = do
    x' <- files
    case find ((== (cs x)) . fst ) x' of
      Just (_, FileInfo fn _ c) -> do
        case c of
          "" -> pure $ Just $ Left (x, [(x, MNE.MyNioFieldErrorNotPresent)])
          _ -> do
            p <- fileDataPath
            let fullImagePath = p ++"/"++ cs fn
            liftIO $ BSL.writeFile fullImagePath c
            pure $ Just $ Right $ FileUpload $ cs fn
      Nothing -> do
        pure $ Just $ Left (x, [(x, MNE.MyNioFieldErrorNotPresent)])

instance FieldGetterErrorKey Identity UTCTimestamp String where
  getFieldErrorKey k _ = pure k

instance FieldGetter Identity UTCTimestamp MNE.MyNioFieldError String  where
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, MNE.MyNioFailedValidation $ "Invalid timestamp: " ++ x)])
      Nothing -> Nothing

instance FieldGetterErrorKey Identity Word16 String where
  getFieldErrorKey k _ = pure k

instance FieldGetter Identity Word16 MNE.MyNioFieldError String  where
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, MNE.MyNioFailedValidation $ "Invalid word16: " ++ x)])
      Nothing -> Nothing

lala :: String -> String -> [(String,String)] -> [(String,String)] -> Maybe [(String,String)]
lala k1 k2 [] [] = Just []
lala k1 k2 _ [] = Nothing
lala k1 k2 [] _ = Nothing
lala k1 k2 ((kk,v):xs) ((kk2,v2):ys) = case
  (stripPrefix k1 kk, stripPrefix k2 kk2) of
    (Just x, Just x') -> if x' == x' then fmap concat $ sequence [Just [(v,v2)] , lala k1 k2 xs ys] else Nothing
    _ -> Nothing

instance Monad m => FieldGetterErrorKey m [(Text,Text)] (String, String) where
  getFieldErrorKey (k1,k2) _ = pure (k1++k2)

instance Monad m => FieldGetter m [(Text,Text)] MNE.MyNioFieldError (String, String)  where
  getField (k1,k2) i = pure $ do
    let zz1 = filter (isPrefixOf k1 .  fst) i :: [(String, String)]
    let zz2 = filter (isPrefixOf k2 .  fst) i
    case lala k1 k2 zz1 zz2 of
      Just x -> Just $ Right $ fmap (both cs) x
      Nothing -> Just $ Left (k1++k2, [(k1, MNE.MyNioFailedValidation "Something went wrong ??? from to redirects")])

instance Monad m => FieldGetterErrorKey m (Maybe Int) String where
  getFieldErrorKey k _ = pure k

instance Monad m => FieldGetter m (Maybe Int) MNE.MyNioFieldError String  where
  getField k i =
    case lookup k i of
      Just x -> case x of
                  "" -> pure $ Just $ Right Nothing
                  x' -> case readMaybe x of
                    Just x'' -> pure $ Just $ Right $ Just x''
                    Nothing -> pure $ Just $ Left (k, [(k, MNE.MyNioFailedValidation $ "Failed to read Maybe Int value for field '" ++ k ++ "': " ++ x)])
      Nothing -> pure $ Nothing

instance FieldGetterErrorKey Identity PostTags String where
  getFieldErrorKey k _ = pure k

instance FieldGetter Identity PostTags MNE.MyNioFieldError String  where
  getField k i = do
    result <- getField k i  -- Get as [String] first
    pure $ case result of
      Just (Right tags) -> Just $ Right $ PostTags $ fmap (cs :: String -> Text) tags
      Just (Left err) -> Just $ Left err
      Nothing -> Nothing
