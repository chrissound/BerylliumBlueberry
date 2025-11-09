{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
module Forms.File where

import Blog
import AppTypes
import NioForm
import NioFormTypes
import Routes as R
import Common
import System.Directory
import Control.Monad

import Lucid
import Forms.Forms2
import Models.File
import AppCommon hiding (File, files)
import MyNioFormHtml
import qualified MyNioFieldError as MNE

postForm :: NioForm MNE.MyNioFieldError
postForm =
  NioForm [
       NioFieldView "" "fileId" emptyError
         NioFieldInputHidden $ NioFieldValS ""
     , NioFieldView "fileTitle" "fileTitle" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
     , NioFieldView "fileEasyId" "fileEasyId" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
     , NioFieldView "fileCreated" "fileCreated" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
     , NioFieldView "fileFile" "fileFile" emptyError
         NioFieldInputFile  $ NioFieldValS ""
  ]

postForm' :: File -> NioForm MNE.MyNioFieldError
postForm' p =
  NioForm [
       NioFieldView "" "fileId" emptyError
         NioFieldInputHidden (NioFieldValS $ show $ fileId p)
     , NioFieldView "fileTitle" "fileTitle" emptyError
         NioFieldInputTextShort (NioFieldValS $ cs $ fileTitle p)
     , NioFieldView "fileEasyId" "fileEasyId" emptyError
         NioFieldInputTextShort (NioFieldValS $ cs $ fileEasyId p)
     , NioFieldView "fileCreated" "fileCreated" emptyError
         NioFieldInputText (NioFieldValS $ show $ fileCreated p)
     , NioFieldView "fileFile" "fileFile" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
  ]

postFormLucid :: NioForm MNE.MyNioFieldError -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateFile)

postEditFormLucid :: Int -> NioForm MNE.MyNioFieldError -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditFile x )


inputFile :: FormInput -> AppAction (Either [FieldEr MNE.MyNioFieldError] File)
inputFile fi = do
  allErrors' <- mconcat <$> sequence (allErrors :: [AppActionT [FieldEr MNE.MyNioFieldError]])
  (first $ const allErrors') <$> ((liftM5 . liftM5) File <$> a <*> b <*> c <*> d <*> e) fi
  where
      allErrors = [
          getFormErrorsM fi [a]
        , getFormErrorsM fi [b]
        , getFormErrorsM fi [c]
        , getFormErrorsM fi [d]
        , getFormErrorsM fi [e]
        ] :: [AppActionT [FieldEr MNE.MyNioFieldError]]
      a = pure <$> fieldValue isPresent ("fileId" :: String)
      b = pure <$> fieldValue isPresent ("fileTitle" :: String)
      c = pure <$> fieldValue isPresent ("fileEasyId" :: String)
      d = pure <$> fieldValue isPresent ("fileCreated" :: String)
      e = fieldValue' isPresent ("fileFile" :: String)


