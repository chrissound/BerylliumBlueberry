{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
module Forms.Image where

import Blog
import AppTypes
import NioForm
import NioFormTypes
import Routes as R
import Common
-- import Control.Monad
-- import System.Directory

import Control.Monad

import Lucid
import Forms.Forms2
import Forms.File
import Models.Image
import Models.File
import AppCommon
import MyNioFormHtml

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateImage)

postEditFormLucid :: Int -> NioForm -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditImage x )

inputImage :: FormInput -> AppAction (Either [FieldEr] Image)
inputImage fi = do
  file' <- inputFile fi
  x <- fieldValue' isPresent "fileFile" fi :: AppAction (Either FieldEr ImageResizedFileUpload)
  case (file', x) of
    (Right (File a b c d _), Right x') -> pure $ pure $ Image a b c d x'
    _ -> do
      liftIO $ pPrint file'
      liftIO $ pPrint x
      error "aaaaaaaaaaaa"


