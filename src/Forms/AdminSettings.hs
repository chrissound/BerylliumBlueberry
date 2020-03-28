{-# LANGUAGE OverloadedStrings #-}
module Forms.AdminSettings where

import Blog

import NioForm
import NioFormTypes
import Routes as R
import AppConfig
-- import Data.Aeson
import Data.Text
import Common
import Data.Bifunctor
import Data.List
-- import Control.Monad

import Control.Monad

import Lucid
import Forms.Forms2
-- import Data.Aeson
import Data.Aeson.Encode.Pretty

import Database.PostgreSQL.Simple

-- data ConfigV = ConfigV String

xxx :: Int -> (Text,Text) -> [NioFieldView]
xxx x (_,v) = [
    NioFieldView (cs $ "Redirect from - " ++ show x) (cs $ "siteRedirectFrom" ++ "-" ++ show x)
      emptyError NioFieldInputTextShort (cs v)
  , NioFieldView (cs $ "Redirect to - " ++ show x) (cs $ "siteRedirectTo" ++ "-" ++ show x)
      emptyError NioFieldInputTextShort (cs v)
  ]

postForm :: (Maybe AppConfig) -> NioForm
postForm conf =
  NioForm $ [
      NioFieldView "Heading"           (cs $ show AppConfigSiteHeading)
        emptyError NioFieldInputTextShort (maybe "" (cs . siteHeading) conf)
    , NioFieldView "Sub heading"       (cs $ show AppConfigSiteSubHeading)
        emptyError NioFieldInputTextShort (maybe "" (cs . siteSubHeading) conf)
     ]
  ++
  [
      NioFieldView "Email"             (cs $ show AppConfigSiteContactEmail)
        emptyError NioFieldInputTextShort (maybe "" (cs . siteContactEmail) conf)
    , NioFieldView "Side bar html"     (cs $ show AppConfigSiteSideBarHtml)
        emptyError NioFieldInputTextShort (maybe "" (cs . siteSideBarHtml) conf)
    , NioFieldView "Extra head html"   (cs $ show AppConfigSiteExtraHeadHtml)
        emptyError NioFieldInputTextShort (maybe "" (cs . siteExtraHeadHtml) conf)
    , NioFieldView "Database host"     (cs $ show AppConfigDatabaseConnectionHost)
        emptyError NioFieldInputTextShort (maybe "" (cs . connectHost . databaseConnection) conf)
    , NioFieldView "Database user"     (cs $ show AppConfigDatabaseConnectionUser)
        emptyError NioFieldInputTextShort (maybe "" (cs . connectUser . databaseConnection) conf)
    , NioFieldView "Database password" (cs $ show AppConfigDatabaseConnectionPassword)
        emptyError NioFieldInputTextShort (maybe "" (cs . connectPassword . databaseConnection) conf)
    , NioFieldView "Database name"     (cs $ show AppConfigDatabaseConnectionName)
        emptyError NioFieldInputTextShort (maybe "" (cs . connectDatabase . databaseConnection) conf)
    , NioFieldView "Database port"     (cs $ show AppConfigDatabaseConnectionPort)
        emptyError NioFieldInputTextShort (maybe "" (show . connectPort . databaseConnection) conf)
  ]
  ++
  maybe [] (mconcat . fmap (\(x,sr) -> xxx x sr) . Data.List.zip [0..] .  siteRedirect) conf
  ++
  maybe (xxx 0 ("","")) (\ac -> xxx (Data.List.length $ siteRedirect ac) ("","")) conf

postFormLucid :: NioForm -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminSettings)

inputDatabaseConnectionPost :: FormInput -> Either [FieldEr] ConnectInfo
inputDatabaseConnectionPost fi = do
  ((first $ const allErrors)
    <$>
    ((liftM5 ConnectInfo) <$> a <*> b <*> c <*> d <*> e)) fi
  where
      allErrors = mconcat $
        [
          getFormErrors fi [a]
        , getFormErrors fi [b]
        , getFormErrors fi [c]
        , getFormErrors fi [d]
        , getFormErrors fi [e]
        ]
      a = myGetField isPresent (show AppConfigDatabaseConnectionHost)
      b = myGetField isPresent (show AppConfigDatabaseConnectionPort)
      c = myGetField isPresent (show AppConfigDatabaseConnectionUser)
      d = myGetField isPresent (show AppConfigDatabaseConnectionPassword)
      e = myGetField isPresent (show AppConfigDatabaseConnectionName)

inputPost :: FormInput -> Either ([FieldEr]) AppConfig
inputPost fi = do
  case inputDatabaseConnectionPost fi of
    Left e' -> Left $ allErrors ++ e'
    Right dbconfig ->
      (first $ const allErrors)
        $
        ((liftM7 AppConfig) <$> a <*> b <*> c <*> d <*> e <*> f <*> (const $ pure dbconfig))  fi

  where
      allErrors = mconcat $ -- (getFormErrors fi . pure <$> [a,b,c,d,e,f])
        [
          getFormErrors fi [a]
        , getFormErrors fi [b]
        , getFormErrors fi [c]
        , getFormErrors fi [d]
        , getFormErrors fi [e]
        , getFormErrors fi [f]
        ]
      a = myGetField isPresent (show AppConfigSiteHeading)
      b = myGetField isPresent (show AppConfigSiteSubHeading)
      c = myff isPresent isPresent "siteRedirectFrom" "siteRedirectTo"
      d = myGetField isPresent (show AppConfigSiteContactEmail)
      e = myGetField isPresent (show AppConfigSiteSideBarHtml)
      f = myGetField isPresent (show AppConfigSiteExtraHeadHtml)

myff :: (Show a, FieldGetter a, Show b, FieldGetter b)
  => NioValidateField a
  -> NioValidateField b
  -> NioFormKey -> NioFormKey -> FormInput -> Either FieldEr [(a, b)]
myff nv nv2 k k2 fi = case (,) <$> myGetFieldArray nv k fi <*> myGetFieldArray nv2 k2 fi of
  Right (x, x') -> pure $ Data.List.zip x x'
  Left e -> Left e

myGetFieldArray :: (Show a, FieldGetter a)
  => NioValidateField a
  -> NioFormKey -> FormInput -> Either FieldEr [a]
myGetFieldArray nv k fi = sequence $ (\x -> myGetField nv x fi) <$> hmm 0
  where
  hmm i = case (Data.List.find ((==) (k ++ "-" ++ show i) . fst) fi) of
    Just x -> [fst x] ++ hmm (i + 1)
    Nothing -> []
