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

--postForm :: NioForm
--postForm =
  --NioForm [
       --NioFieldView "" "imageId" emptyError
         --NioFieldInputHidden (NioFieldValS "")
     --, NioFieldView "imageTitle" "imageTitle" emptyError
         --NioFieldInputTextShort (NioFieldValS "")
     --, NioFieldView "imageEasyId" "imageEasyId" emptyError
         --NioFieldInputTextShort (NioFieldValS "")
     --, NioFieldView "imageCreated" "imageCreated" emptyError
         --NioFieldInputTextShort (NioFieldValS "")
     --, NioFieldView "imageFile" "imageFile" emptyError
         --NioFieldInputFile (NioFieldValS "")
  --]

--postForm' :: Image -> NioForm
--postForm' p =
  --NioForm [
       --NioFieldView "" "imageId" emptyError
         --NioFieldInputHidden (NioFieldValS $ show $ idInteger $ imageId p)
     --, NioFieldView "imageTitle" "imageTitle" emptyError
         --NioFieldInputTextShort (NioFieldValS $ cs $ imageTitle p)
     --, NioFieldView "imageEasyId" "imageEasyId" emptyError
         --NioFieldInputTextShort (NioFieldValS $ cs $ imageEasyId p)
     --, NioFieldView "imageCreated" "imageCreated" emptyError
         --NioFieldInputText (NioFieldValS $ show $ imageCreated p)
     --, NioFieldView "imageFile" "imageFile" emptyError
         --NioFieldInputTextShort (NioFieldValS "")
  --]

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateImage)

postEditFormLucid :: Int -> NioForm -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditImage x )

-- inputTest :: FormInput -> AppAction (Either [FieldEr] Image)
-- inputTest fi = do
--     allErrors' <- mconcat <$> sequence (allErrors :: [AppActionT [FieldEr]])
--     (first $ const allErrors')
--       <$>
--       ((liftM5 . liftM5) Image <$> a <*> b <*> c <*> d <*> e)  fi
--   where
--     a = undefined
--     b = undefined
--     c = undefined
--     d = undefined
--     e = undefined
--     -- f = myGetFile always "imageFile"
--     allErrors = []


-- inputTest :: FormInput -> IO (Either [String] (Int, Int))
-- inputTest fi = do
--     allErrors' <- undefined :: IO [String]
--     undefined
--     <$> ((liftM2 ) (,) <$> undefined <*> undefined) fi


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


