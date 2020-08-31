{-# LANGUAGE OverloadedStrings #-}
module Forms.Post2 where

import Blog

import NioForm
import NioFormTypes
import Routes as R
import Common
import Control.Monad

--import Control.Monad

import Lucid
import Forms.Forms2
import MyNioFormHtml

--import DigestiveFunctorsPostgreSQL
--import Forms.Forms
import Models.Post as MdlP
-- import Models.Comment as MdlC
-- import NioFormExtra
--import qualified Routes as R

postForm :: NioForm
postForm =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden (NioFieldValS "")
     , NioFieldView "postTitle" "postTitle" emptyError
         NioFieldInputTextShort (NioFieldValS "")
     , NioFieldView "postEasyId" "postEasyId" emptyError
         NioFieldInputTextShort (NioFieldValS "")
     , NioFieldView "postBody" "postBody" emptyError
         NioFieldInputText (NioFieldValS "")
     , NioFieldView "postCreated" "postCreated" emptyError
         NioFieldInputTextShort (NioFieldValS "")
  ]

postForm' :: Post -> NioForm
postForm' p =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden (NioFieldValS $ show $ idInteger $ postId p)
     , NioFieldView "postTitle" "postTitle" emptyError
         NioFieldInputTextShort (NioFieldValS $ cs $ postTitle p)
     , NioFieldView "postEasyId" "postEasyId" emptyError
         NioFieldInputTextShort (NioFieldValS $ cs $ postEasyId p)
     , NioFieldView "postBody" "postBody" emptyError
         NioFieldInputText (NioFieldValS $ cs $ postBody p)
     , NioFieldView "postCreated" "postCreated" emptyError
         NioFieldInputTextShort (NioFieldValS $ show $ postCreated p)
  ]

inputPost :: FormInput -> Either ([FieldEr]) Post
inputPost fi = do
  (((liftM5 Post) <$> a <*> b <*> c <*> d <*> e) >>= \case
    Right x' -> pure $ pure x'
    Left _ -> (\_ -> do
                  Left allErrors
      )) fi
  where
      allErrors = mconcat [
                       getFormErrors fi [a]
                     , getFormErrors fi [b]
                     , getFormErrors fi [c]
                     , getFormErrors fi [d]
                     , getFormErrors fi [e]
                     ]
      a = fieldValue isPresent "postId"
      b = fieldValue isPresent "postTitle"
      c = fieldValue (allRules [
                         minLength 3
                         ]) "postBody"
      d = fieldValue isPresent "postCreated"
      e = fieldValue (allRules [
                         minLength 3
                         ]) "postEasyId"

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreatePost)

postEditFormLucid :: Int ->  NioForm -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditPost x)

pageFormLucid :: NioForm -> Html ()
pageFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateSinglePage)
