{-# LANGUAGE OverloadedStrings #-}
module Forms.Login where

--import Web.Forms.Common

import Lucid
import qualified Data.Text as T
import Control.Applicative

import Routes as R
-- import Forms.Forms
import Forms.Forms2
import NioForm
import NioFormTypes
import Blog

data LoginRequest
   = LoginRequest
   { lr_user :: T.Text
   , lr_password :: T.Text
   } deriving (Show)

loginForm' :: NioForm
loginForm' = 
  NioForm [
       NioFieldView "Username" "user" emptyError
         NioFieldInputTextShort ""
     , NioFieldView "Password" "password" emptyError
         NioFieldInputTextShort ""
     ]

inputLogin' :: Form' LoginRequest
inputLogin' = do
  (liftA2 LoginRequest) <$> a <*> b >>= \case
    Right x' -> pure $ pure x'
    Left _ -> (\z -> Left $ mconcat [
        getFormErrors z [a]
      , getFormErrors z [b]
      ])
  where
    a = myGetField isPresent "user"
    b = myGetField isPresent "password"

-- loginForm :: Monad m => Form (Html ()) m LoginRequest
-- loginForm =
--     LoginRequest <$> "user" .: text Nothing
--                  <*> "password" .: text Nothing

loginFormLucid' :: NioForm -> Html ()
loginFormLucid' nf = nioformHtml $ NioFormHtml nf (R.Login)
