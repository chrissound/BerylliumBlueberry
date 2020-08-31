{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module AppTypes where

import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
import Server

type App = ScottySessionT IO
type AppServer a = ScottyT L.Text App a
type AppAction a = ActionT L.Text App a
type AppActionT = ActionT L.Text App

data FileUpload = FileUpload String deriving Show
data ImageResizedFileUpload = ImageResizedFileUpload String deriving Show
