{-# OPTIONS -Wno-orphans #-}

module AppTypeDb where

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import AppTypes

instance ToField FileUpload where toField (FileUpload x) = toField x
instance FromField FileUpload where
  fromField f dat = FileUpload <$> fromField f dat 

instance FromField ImageResizedFileUpload where
  fromField f dat = ImageResizedFileUpload <$> fromField f dat 
instance ToField ImageResizedFileUpload where toField (ImageResizedFileUpload x) = toField x
