{-# LANGUAGE OverloadedStrings #-}
module Forms.Post2 where

import Blog

import NioForm
import NioFormTypes
import Routes as R
import Common
import Control.Monad
import Lucid
import Forms.Forms2
import MyNioFormHtml
import Models.Post as MdlP

postForm :: NioForm
postForm = postForm'' Nothing

postForm' :: Post -> NioForm
postForm' p = postForm'' $ Just p

postForm'' :: Maybe Post -> NioForm 
postForm'' mp =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden $ maybe (NioFieldValS "") (NioFieldValS . show . idInteger . postId) mp
     , NioFieldView "postTitle" "postTitle" emptyError
         NioFieldInputTextShort $ maybe (NioFieldValS "") (NioFieldValS . cs . postTitle) mp
     , NioFieldView "postEasyId" "postEasyId" emptyError
         NioFieldInputTextShort $ maybe (NioFieldValS "") (NioFieldValS . cs . postEasyId) mp
     , NioFieldView "postBody" "postBody" emptyError
         NioFieldInputText $ maybe (NioFieldValS "") (NioFieldValS . cs . postBody) mp
     , NioFieldView "postTags" "postTags" emptyError
         -- NioFieldInputText $ maybe (NioFieldValS "") (NioFieldValS . cs . postBody) mp

        (NioFieldInputLabled True [("Test","hmm"), ("Test123","123hmm")])
        (NioFieldValM $ maybe [] (const []) mp)
     , NioFieldView "postCreated" "postCreated" emptyError
         NioFieldInputTextShort $ maybe (NioFieldValS "") (NioFieldValS . show . postCreated) mp

  ]

inputPost :: FormInput -> Either ([FieldEr]) Post
inputPost fi = do
  (((liftM6 Post) <$> a <*> b <*> c <*> d <*> e <*> f) >>= \case
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
                     , getFormErrors fi [f]
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
      f = fieldValue always' "postTags"

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreatePost)

postEditFormLucid :: Int ->  NioForm -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditPost x)

pageFormLucid :: NioForm -> Html ()
pageFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateSinglePage)
