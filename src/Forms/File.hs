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

-- import Control.Monad
-- import System.Directory

import Control.Monad

import Lucid
import Forms.Forms2
import Models.File
import AppCommon hiding (File, files)

postForm :: NioForm
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

postForm' :: File -> NioForm
postForm' p =
  NioForm [
       NioFieldView "" "fileId" emptyError
         NioFieldInputHidden (NioFieldValS $ show $ idInteger $ fileId p)
     , NioFieldView "fileTitle" "fileTitle" emptyError
         NioFieldInputTextShort (NioFieldValS $ cs $ fileTitle p)
     , NioFieldView "fileEasyId" "fileEasyId" emptyError
         NioFieldInputTextShort (NioFieldValS $ cs $ fileEasyId p)
     , NioFieldView "fileCreated" "fileCreated" emptyError
         NioFieldInputText (NioFieldValS $ show $ fileCreated p)
     , NioFieldView "fileFile" "fileFile" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
  ]

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateFile)

postEditFormLucid :: Int -> NioForm -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditFile x )

-- inputTest :: FormInput -> AppAction (Either [FieldEr] File)
-- inputTest fi = do
--     allErrors' <- mconcat <$> sequence (allErrors :: [AppActionT [FieldEr]])
--     (first $ const allErrors')
--       <$>
--       ((liftM5 . liftM5) File <$> a <*> b <*> c <*> d <*> e)  fi
--   where
--     a = undefined
--     b = undefined
--     c = undefined
--     d = undefined
--     e = undefined
--     -- f = myGetFile always "fileFile"
--     allErrors = []


-- inputTest :: FormInput -> IO (Either [String] (Int, Int))
-- inputTest fi = do
--     allErrors' <- undefined :: IO [String]
--     undefined
--     <$> ((liftM2 ) (,) <$> undefined <*> undefined) fi


inputFile :: FormInput -> AppAction (Either [FieldEr] File)
inputFile fi = do
  allErrors' <- mconcat <$> sequence (allErrors :: [AppActionT [FieldEr]])
  (first $ const allErrors') <$> ((liftM5 . liftM5) File <$> a <*> b <*> c <*> d <*> e) fi
  where
      allErrors = [
          getFormErrorsM fi [a]
        , getFormErrorsM fi [b]
        , getFormErrorsM fi [c]
        , getFormErrorsM fi [d]
        , getFormErrorsM fi [e]
        ] :: [AppActionT [FieldEr]]
      a = pure <$> fieldValue isPresent "fileId"
      b = pure <$> fieldValue isPresent "fileTitle"
      c = pure <$> fieldValue isPresent "fileEasyId"
      d = pure <$> fieldValue isPresent "fileCreated"
      e = fieldValue' isPresent "fileFile"


