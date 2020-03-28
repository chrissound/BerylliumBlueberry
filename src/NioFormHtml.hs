{-# OPTIONS -Wno-dodgy-exports #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}
module NioFormHtml where

import Lucid
import NioForm
import NioFormM
import NioFormTypes
import NioFormInstances
import NioFormExtra
import Data.String
import Control.Monad
import Data.String.Conversions
import Routes as R
import AppTypes
import Template.Base
-- import AppRender
import NioFormErrors
import Common

data NioFormHtml = NioFormHtml {
    nioForm :: NioForm
  , action :: PublicRoute
  }


friendlyError :: NioFieldError -> Html ()
friendlyError = p_ . fromString . friendlyError'

friendlyError' :: NioFieldError -> String
friendlyError' (NioFieldErrorV a) = show a

nioformHtml :: NioFormHtml -> Html ()
nioformHtml (NioFormHtml (NioForm nf) action') = do
  with form_ [
    method_ "post"
    , action_ . cs $ R.renderPublicUrl $ action'
    , class_ "blockInputs"
    , enctype_ "multipart/form-data"
    ] $ do
      forM_ nf $ \nfv@(NioFieldView a b ers nfi _) -> do
        when (length ers > 0)
          (alertBox (BootAlertWarn)
            $ (p_ $ fromString ("Errors for " ++ (cs $ fvLabel nfv)))
            <> (mconcat $ friendlyError <$> ers)
          )
        <>
        case nfi of
          NioFieldInputFile -> do
            input_ [
                type_ "file"
              , name_ (fromString $ cs $ fvId nfv)
              , value_ (fromString $ cs $ fvValue nfv)
              ]
          NioFieldInputHidden -> do
            input_ [
                type_ "hidden"
              , name_ (fromString $ cs $ fvId nfv)
              , value_ (fromString $ cs $ fvValue nfv)
              ]
          NioFieldInputText -> do
            label_ $ fromString $ cs $ fvLabel nfv
            textarea_ [type_ "text", name_ (fromString $ cs $ fvId nfv)] $ fromString $ fvValue nfv
            br_ []
          NioFieldInputTextShort -> do
            label_ $ fromString $ cs $ fvLabel nfv
            input_ [
                type_ "text"
              , name_ (fromString $ cs $ fvId nfv)
              , value_ (fromString $ cs $ fvValue nfv)
              ]
            br_ []
          _ -> error "Invalid input field..."
      input_ [type_ "submit"]

myRunForm ::
  NioForm
  -> (FormInput -> Either [FieldEr] a)
  -> FormInput
  -> Either NioForm a
myRunForm nf = runInputForm nf

myRunFormM :: (Monad m) =>
  NioForm
  -> (FormInput -> m (Either [FieldEr] a))
  -> FormInput
  -> m (Either NioForm a)
myRunFormM nf = runInputFormM nf
