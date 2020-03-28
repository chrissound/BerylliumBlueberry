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
         NioFieldInputHidden ""
     , NioFieldView "postTitle" "postTitle" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "postEasyId" "postEasyId" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "postBody" "postBody" emptyError
         NioFieldInputText ""
     , NioFieldView "postCreated" "postCreated" emptyError
         NioFieldInputTextShort ""
  ]

postForm' :: Post -> NioForm
postForm' p =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden (show $ idInteger $ postId p)
     , NioFieldView "postTitle" "postTitle" emptyError
         NioFieldInputTextShort (cs $ postTitle p)
     , NioFieldView "postEasyId" "postEasyId" emptyError
         NioFieldInputTextShort (cs $ postEasyId p)
     , NioFieldView "postBody" "postBody" emptyError
         NioFieldInputText (cs $ postBody p)
     , NioFieldView "postCreated" "postCreated" emptyError
         NioFieldInputTextShort (show $ postCreated p)
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
      a = myGetField isPresent "postId"
      b = myGetField isPresent "postTitle"
      c = myGetField (allRules [
                         minLength 3
                         ]) "postBody"
      d = myGetField isPresent "postCreated"
      e = myGetField (allRules [
                         minLength 3
                         ]) "postEasyId"

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreatePost)

postEditFormLucid :: Int ->  NioForm -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditPost x)

pageFormLucid :: NioForm -> Html ()
pageFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateSinglePage)
