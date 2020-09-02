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
import Database.PostgreSQL.ORM
import Database.PostgreSQL.ORM.Model
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

instance FieldGetter AppActionT ImageResizedFileUpload String where
  getFieldErrorKey k _ = pure $ k
  getField x i = do
    x' <- files
    case find ((== (cs x)) . fst ) (x') of
      Just (_, FileInfo fn _ c) -> do
        case (c) of
          "" -> pure $ Just $ Left (x, [(x, NioFieldErrorV "No file uploaded")])
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
              e -> pure $ Just $ Left $ (x, [(x, NioFieldErrorV $ "Unable to resize image: " ++ show e)])
      Nothing -> do
        pure $ Just $ Left (x, [(x, NioFieldErrorV "No file uploaded")])

instance FieldGetter (AppActionT) (FileUpload) String where
  getFieldErrorKey k _ = pure $ k
  getField x i = do
    x' <- files
    case find ((== (cs x)) . fst ) x' of
      Just (_, FileInfo fn _ c) -> do
        case c of
          "" -> pure $ Just $ Left (x, [(x, NioFieldErrorV "No file uploaded")])
          _ -> do
            p <- fileDataPath
            let fullImagePath = p ++"/"++ cs fn
            liftIO $ BSL.writeFile fullImagePath c
            pure $ Just $ Right $ FileUpload $ cs fn
      Nothing -> do
        pure $ Just $ Left (x, [(x, NioFieldErrorV "No file uploaded")])

instance FieldGetter Identity UTCTimestamp String  where
  getFieldErrorKey k _ = pure $ k
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, NioFieldErrorV $ "Invalid timestamp: " ++ x)])
      Nothing -> Nothing

instance FieldGetter Identity Word16 String  where
  getFieldErrorKey k _ = pure $ k
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, NioFieldErrorV $ "Invalid timestamp: " ++ x)])
      Nothing -> Nothing

lala :: String -> String -> [(String,String)] -> [(String,String)] -> Maybe [(String,String)]
lala k1 k2 [] [] = Just []
lala k1 k2 _ [] = Nothing
lala k1 k2 [] _ = Nothing
lala k1 k2 ((kk,v):xs) ((kk2,v2):ys) = case
  (stripPrefix k1 kk, stripPrefix k2 kk2) of
    (Just x, Just x') -> if x' == x' then fmap concat $ sequence [Just [(v,v2)] , lala k1 k2 xs ys] else Nothing
    _ -> Nothing

instance Monad m => FieldGetter m [(Text,Text)] (String, String)  where
  getFieldErrorKey k _ = pure $ []
  getField (k1,k2) i = pure $ do
    let zz1 = filter (isPrefixOf k1 .  fst) i :: [(String, String)]
    let zz2 = filter (isPrefixOf k2 .  fst) i
    case lala k1 k2 zz1 zz2 of
      Just x -> Just $ Right $ fmap (both cs) x
      Nothing -> Just $ Left (k1++k2, [(k1, NioFieldErrorV "Something went wrong ??? from to redirects")])

instance (Monad m) => FieldGetter m DBKeyType String where
  getFieldErrorKey k _ = pure k
  getField k i = pure $ do
    case lookup k i of
      Just x -> case readMaybe x of 
                  Just x' -> Just $ Right x'
                  Nothing -> Just $ Left (k, [(k, NioFieldErrorV $ "Failed to read value: " ++ x)])
      Nothing -> Nothing

instance Monad m => FieldGetter m DBKey String  where
  getFieldErrorKey k _ = pure k
  getField k i = 
    case lookup k i of
      Just x -> case x of 
                  "" -> pure $ Just $ Right NullKey
                  x' -> case readMaybe x of
                    Just x'' -> pure $ Just $ Right $ DBKey x''
                    Nothing -> pure $ Just $ Left (k, [(k, NioFieldErrorV $ "Failed to read value: " ++ x)])
      Nothing -> pure $ Nothing

instance FieldGetter Identity (DBRef a) String where
  getField k i = fmap (fmap (fmap (DBRef))) (getField k i)
  getFieldErrorKey k _ = pure k

instance FieldGetter Identity PostTags String  where
  getField k i = fmap (fmap (fmap (PostTags . id . (fmap (cs :: String -> Text))))) (getField k i)
  getFieldErrorKey k _ = pure k
