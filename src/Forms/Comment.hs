{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-matches #-}

module Forms.Comment where

import Blog

import NioForm
import NioFormTypes
import Routes as R (RouteUrl(CreateCommentOnPost))
import Common

--import Control.Monad

import Lucid
import Forms.Forms2
import MyNioFormHtml

--import DigestiveFunctorsPostgreSQL
--import Forms.Forms

import NioForm
import NioFormTypes
import Models.Post as MdlP
import Models.Comment as MdlC
import qualified MyNioFieldError as MNE
-- import NioFormExtra
--import qualified Routes as R

commentForm :: NioForm MNE.MyNioFieldError
commentForm =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden  $ NioFieldValS ""
     , NioFieldView "Your name:" "authorAlias" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
     , NioFieldView "Comment:" "commentBody" emptyError
         NioFieldInputText  $ NioFieldValS ""
    ,
       NioFieldView "" "commentCreated" emptyError
         NioFieldInputHidden (NioFieldValS "")
    ,
       NioFieldView "" "commentId" emptyError
         NioFieldInputHidden (NioFieldValS "")
  ]

commentForm' :: Post ->  NioForm MNE.MyNioFieldError
commentForm' p =
  NioForm [
       NioFieldView "" "postId" emptyError
         NioFieldInputHidden (NioFieldValS $ show  $ MdlP.postId p)
     , NioFieldView "Your name:" "authorAlias" emptyError
         NioFieldInputTextShort  $ NioFieldValS ""
     , NioFieldView "Comment:" "commentBody" emptyError
         NioFieldInputText  $ NioFieldValS ""
    ,
       NioFieldView "" "commentCreated" emptyError
         NioFieldInputHidden (NioFieldValS "")
    ,
       NioFieldView "" "commentId" emptyError
         NioFieldInputHidden (NioFieldValS "")
  ]

inputComment :: FormInput -> Either ([FieldEr MNE.MyNioFieldError]) Comment
inputComment = do
  ((liftM6 Comment) <$> a <*> b <*> c <*> d <*> e <*> f) >>= \case
    Right x' -> pure $ pure x'
    Left _ -> (\z -> do
                  Left $ mconcat [
                       getFormErrors z [a]
                     , getFormErrors z [b]
                     , getFormErrors z [c]
                     , getFormErrors z [d]
                     , getFormErrors z [e]
                     , getFormErrors z [f]
                     ]
      )
  where
      a = fieldValue isPresent ("commentId" :: String)
      b = fieldValue isPresent ("postId" :: String)
      c = fieldValue (allRules [
                         minLength 3
                         ]) ("commentBody" :: String)
      d = fieldValue isPresent ("authorAlias" :: String)
      e = const $ pure False
      f = fieldValue isPresent ("postCreated" :: String)


commentFormLucid :: NioForm MNE.MyNioFieldError -> Html ()
commentFormLucid nf = nioformHtml $ NioFormHtml nf R.CreateCommentOnPost
