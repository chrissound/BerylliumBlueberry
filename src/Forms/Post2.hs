{-# LANGUAGE OverloadedStrings #-}
module Forms.Post2 where

import Blog

import NioForm
import NioFormTypes
import Routes as R
import Common
import Control.Monad
import Data.Maybe (fromMaybe)
import Lucid
import Forms.Forms2
import MyNioFormHtml
import Models.Post as MdlP
import qualified MyNioFieldError as MNE

postForm :: NioForm MNE.MyNioFieldError
postForm = postForm'' Nothing

postForm' :: Post -> NioForm MNE.MyNioFieldError
postForm' p = postForm'' $ Just p

postForm'' :: Maybe Post -> NioForm MNE.MyNioFieldError 
postForm'' mp =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden $ maybe (NioFieldValS "") (NioFieldValS . show . postId) mp
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

inputPost :: FormInput -> Either ([FieldEr MNE.MyNioFieldError]) Post
inputPost fi = do
  (((liftM6 Post) <$> (fmap (fromMaybe 0) <$> a) <*> b <*> c <*> d <*> e <*> f) >>= \case
    Right x' -> pure $ pure x'
    Left _ -> (\_ -> do
                  Left allErrors
      )) fi
  where
      allErrors = mconcat [
                       getFormErrors fi [b]
                     , getFormErrors fi [c]
                     , getFormErrors fi [d]
                     , getFormErrors fi [e]
                     , getFormErrors fi [f]
                     ]
      a = fieldValue always' ("postId" :: String)
      b = fieldValue isPresent ("postTitle" :: String)
      c = fieldValue (allRules [
                         minLength 3
                         ]) ("postBody" :: String)
      d = fieldValue isPresent ("postCreated" :: String)
      e = fieldValue (allRules [
                         minLength 3
                         ]) ("postEasyId" :: String)
      f = fieldValue always' ("postTags" :: String)

postFormLucid :: NioForm MNE.MyNioFieldError -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreatePost)

postEditFormLucid :: Int ->  NioForm MNE.MyNioFieldError -> Html ()
postEditFormLucid x nf = nioformHtml $ NioFormHtml nf (R.AdminEditPost x)

pageFormLucid :: NioForm MNE.MyNioFieldError -> Html ()
pageFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminCreateSinglePage)
