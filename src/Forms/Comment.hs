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

--import DigestiveFunctorsPostgreSQL
--import Forms.Forms
import Models.Post as MdlP
import Models.Comment as MdlC
-- import NioFormExtra
--import qualified Routes as R

commentForm :: NioForm
commentForm =
  NioForm [
       NioFieldView "" "commentId" emptyError
         NioFieldInputHidden ""
     , NioFieldView "" "postId" emptyError
         NioFieldInputHidden ""
     , NioFieldView "Your name:" "authorAlias" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "Comment:" "commentBody" emptyError
         NioFieldInputText ""
  ]

commentForm' :: Post ->  NioForm
commentForm' p =
  NioForm [
       NioFieldView "" "commentId" emptyError
         NioFieldInputHidden ""
     , NioFieldView "" "postId" emptyError
         NioFieldInputHidden (show  $ MdlP.postId p)
     , NioFieldView "Your name:" "authorAlias" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "Comment:" "commentBody" emptyError
         NioFieldInputText ""
  ]

inputComment :: FormInput -> Either ([FieldEr]) Comment
inputComment = do
  ((liftM6 Comment) <$> a <*> b <*> c <*> d <*> e <*> f) >>= \case
    Right x' -> pure $ pure x'
    Left _ -> (\z -> do
                  Left $ mydbg "inputComment Left" $ mconcat [
                       getFormErrors z [a]
                     , getFormErrors z [b]
                     , getFormErrors z [c]
                     , getFormErrors z [d]
                     , getFormErrors z [e]
                     , getFormErrors z [f]
                     ]
      )
  where
      a = myGetField isPresent "commentId"
      b = myGetField isPresent "postId"
      c = myGetField (allRules [
                         minLength 3
                         ]) "commentBody"
      d = myGetField isPresent "authorAlias"
      e = myGetField isPresent "approved"
      f = myGetField isPresent "postCreated"


commentFormLucid :: NioForm -> Html ()
commentFormLucid nf = nioformHtml $ NioFormHtml nf (R.CreateCommentOnPost)
