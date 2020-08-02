{-# OPTIONS -Wno-dodgy-exports #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module NioFormHtml where

import Lucid
import NioForm
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
      nioformHtmlFields nf
      input_ [type_ "submit"]

nioformHtmlFields :: [NioFieldView] -> Html ()
nioformHtmlFields nf = do
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
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ (fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))
              ]
          NioFieldInputDigit -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            input_ [
                type_ "text"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ (fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))
              ]
          NioFieldInputHidden -> do
            input_ [
                type_ "hidden"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ $ fscs (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
              ]
          NioFieldInputText -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            textarea_
              [ type_ "text"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              ] $ fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
            br_ []
          NioFieldInputTextShort -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            input_ [
                type_ "text"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              , value_ (fscs $ (case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))
              ]
            br_ []
          NioFieldInputBool c -> do
            with label_ 
              [for_ ((fscs $ case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s))]
              $ fscs $ fvLabel nfv
            input_ $ (if c then (checked_ :) else id) [
                type_ "checkbox"
              , name_ (fscs $ fvId nfv)
              , id_ (fscs $ case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
              , value_ (fscs $ case fvValue nfv of; NioFieldValS s -> s; NioFieldValM s -> unlines s)
              ]
            br_ []
          NioFieldInputLabled multi kv -> do
            with label_ 
              [for_ (fscs $ fvId nfv)]
              $ fscs $ fvLabel nfv
            _ <- with select_ ([
                name_ (fscs $ fvId nfv)
              , id_ (fscs $ fvId nfv)
              ] ++ [multiple_ "multiple" | multi])
              $ sequence $ fmap
                (\(x',y') -> do
                  let y'' = ((case fvValue nfv of; NioFieldValS s -> pure s; NioFieldValM s -> s)) :: [String]
                  (with option_ $ [value_ y'] ++ [ selected_ "" | elem (cs y') y''])
                    $ fscs x'
                )
                kv
            br_ []

fscs :: (ConvertibleStrings a String, IsString b) => a -> b
fscs = fromString . cs
