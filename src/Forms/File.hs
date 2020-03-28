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
         NioFieldInputHidden ""
     , NioFieldView "fileTitle" "fileTitle" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "fileEasyId" "fileEasyId" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "fileCreated" "fileCreated" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "fileFile" "fileFile" emptyError
         NioFieldInputFile ""
  ]

postForm' :: File -> NioForm
postForm' p =
  NioForm [
       NioFieldView "" "fileId" emptyError
         NioFieldInputHidden (show $ idInteger $ fileId p)
     , NioFieldView "fileTitle" "fileTitle" emptyError
         NioFieldInputTextShort (cs $ fileTitle p)
     , NioFieldView "fileEasyId" "fileEasyId" emptyError
         NioFieldInputTextShort (cs $ fileEasyId p)
     , NioFieldView "fileCreated" "fileCreated" emptyError
         NioFieldInputText (show $ fileCreated p)
     , NioFieldView "fileFile" "fileFile" emptyError
         NioFieldInputTextShort ""
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
  currentFiles <- fileDataPath >>= liftIO . getDirectoryContents
  let
  xxx <- sequence (allErrors currentFiles)
  liftIO $ print xxx
  let allErrors' = mconcat xxx
  f' <- f fi
  case f' of
    Right _ ->
      (first $ const $ allErrors')
      <$>
      ((liftM5 . liftM5) File <$> a <*> b <*> c <*> d <*> (e currentFiles))  fi
    Left _ -> do
      pure $ Left $ allErrors'
  where
      allErrors currentFiles' = [
          getFormErrorsM fi [a]
        , getFormErrorsM fi [b]
        , getFormErrorsM fi [c]
        , getFormErrorsM fi [d]
        , getFormErrorsM fi [e currentFiles']
        , getFormErrorsM fi [f]
        ] 
      a = pure <$> myGetField isPresent "fileId"
      b = pure <$> myGetField isPresent "fileTitle"
      c = pure <$> myGetField isPresent "fileEasyId"
      d = pure <$> myGetField isPresent "fileCreated"
      e currentFiles' = myGetFileName
            (allRules
              [
                  isPresent
                , isEq (\(FileUploadName x) -> not $ flip (any . (==)) currentFiles' x) "A file with this name already exists."
                ]
              )
            "fileFile"
      f = myGetFile always "fileFile"
