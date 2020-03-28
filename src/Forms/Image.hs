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
import Models.Image
import AppCommon

postForm :: NioForm
postForm =
  NioForm [
       NioFieldView "" "imageId" emptyError
         NioFieldInputHidden ""
     , NioFieldView "imageTitle" "imageTitle" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "imageEasyId" "imageEasyId" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "imageCreated" "imageCreated" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "imageFile" "imageFile" emptyError
         NioFieldInputFile ""
  ]

postForm' :: Image -> NioForm
postForm' p =
  NioForm [
       NioFieldView "" "imageId" emptyError
         NioFieldInputHidden (show $ idInteger $ imageId p)
     , NioFieldView "imageTitle" "imageTitle" emptyError
         NioFieldInputTextShort (cs $ imageTitle p)
     , NioFieldView "imageEasyId" "imageEasyId" emptyError
         NioFieldInputTextShort (cs $ imageEasyId p)
     , NioFieldView "imageCreated" "imageCreated" emptyError
         NioFieldInputText (show $ imageCreated p)
     , NioFieldView "imageFile" "imageFile" emptyError
         NioFieldInputTextShort ""
  ]

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
  allErrors' <- mconcat <$> sequence (allErrors :: [AppActionT [FieldEr]])
  f' <- f fi
  case f' of
    Right _ ->
      (first $ const allErrors')
      <$>
      ((liftM5 . liftM5) Image <$> a <*> b <*> c <*> d <*> e)  fi
    Left _ -> pure $ Left allErrors'
  where
      allErrors = [
          getFormErrorsM fi [a]
        , getFormErrorsM fi [b]
        , getFormErrorsM fi [c]
        , getFormErrorsM fi [d]
        , getFormErrorsM fi [d]
        , getFormErrorsM fi [f]
        ] :: [AppActionT [FieldEr]]
      a = pure <$> myGetField isPresent "imageId"
      b = pure <$> myGetField isPresent "imageTitle"
      c = pure <$> myGetField isPresent "imageEasyId"
      d = pure <$> myGetField isPresent "imageCreated"
      e = myGetFileName isPresent "imageFile"
      f = myGetFileImageResize always "imageFile"


