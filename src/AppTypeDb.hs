{-# OPTIONS -Wno-orphans #-}

module AppTypeDb where

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField

import AppTypes

instance ToField FileUploadName where toField (FileUploadName x) = toField x
instance FromField FileUploadName where
  fromField f dat = FileUploadName <$> fromField f dat 
