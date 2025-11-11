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

import MyNioFormHtml
import Lucid
import Forms.Forms2
-- import Data.Aeson
import Data.Aeson.Encode.Pretty

import Database.PostgreSQL.Simple
import qualified MyNioFieldError as MNE

-- Helper function for lifting 9 arguments
liftM9 :: Monad m => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m a8 -> m a9 -> m r
liftM9 f m1 m2 m3 m4 m5 m6 m7 m8 m9 = do
  x1 <- m1
  x2 <- m2
  x3 <- m3
  x4 <- m4
  x5 <- m5
  x6 <- m6
  x7 <- m7
  x8 <- m8
  x9 <- m9
  return (f x1 x2 x3 x4 x5 x6 x7 x8 x9)

-- data ConfigV = ConfigV String

xxx :: Int -> (Text,Text) -> [NioFieldView MNE.MyNioFieldError]
xxx x (f,t) = [
    NioFieldView (cs $ "Redirect from - " ++ show x) (cs $ "siteRedirectFrom" ++ "-" ++ show x)
      emptyError NioFieldInputTextShort (NioFieldValS $ cs f)
  , NioFieldView (cs $ "Redirect to - " ++ show x) (cs $ "siteRedirectTo" ++ "-" ++ show x)
      emptyError NioFieldInputTextShort (NioFieldValS $ cs t)
  ]

postForm :: (Maybe AppConfig) -> NioForm MNE.MyNioFieldError
postForm conf =
  NioForm $ [
      NioFieldView "Heading"           (cs $ show AppConfigSiteHeading)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . siteHeading) conf)
    , NioFieldView "Sub heading"       (cs $ show AppConfigSiteSubHeading)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . siteSubHeading) conf)
     ]
  ++
  [
      NioFieldView "Email"             (cs $ show AppConfigSiteContactEmail)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . siteContactEmail) conf)
    , NioFieldView "Side bar html"     (cs $ show AppConfigSiteSideBarHtml)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . siteSideBarHtml) conf)
    , NioFieldView "Extra head html"   (cs $ show AppConfigSiteExtraHeadHtml)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . siteExtraHeadHtml) conf)
    , NioFieldView "Database host"     (cs $ show AppConfigDatabaseConnectionHost)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . connectHost . databaseConnection) conf)
    , NioFieldView "Database user"     (cs $ show AppConfigDatabaseConnectionUser)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . connectUser . databaseConnection) conf)
    , NioFieldView "Database password" (cs $ show AppConfigDatabaseConnectionPassword)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . connectPassword . databaseConnection) conf)
    , NioFieldView "Database name"     (cs $ show AppConfigDatabaseConnectionName)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (cs . connectDatabase . databaseConnection) conf)
    , NioFieldView "Database port"     (cs $ show AppConfigDatabaseConnectionPort)
        emptyError NioFieldInputTextShort (NioFieldValS $ maybe "" (show . connectPort . databaseConnection) conf)
  ]
  ++
  maybe [] (mconcat . fmap (\(x,sr) -> xxx x sr) . Data.List.zip [0..] .  siteRedirect) conf
  ++
  maybe (xxx 0 ("","")) (\ac -> xxx (Data.List.length $ siteRedirect ac) ("","")) conf

postFormLucid :: NioForm MNE.MyNioFieldError -> Html ()
postFormLucid nf = nioformHtml $ NioFormHtml nf (R.AdminSettings)

inputDatabaseConnectionPost :: FormInput -> Either [FieldEr MNE.MyNioFieldError] ConnectInfo
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
      a = fieldValue isPresent (show AppConfigDatabaseConnectionHost)
      b = fieldValue isPresent (show AppConfigDatabaseConnectionPort)
      c = fieldValue isPresent (show AppConfigDatabaseConnectionUser)
      d = fieldValue isPresent (show AppConfigDatabaseConnectionPassword)
      e = fieldValue isPresent (show AppConfigDatabaseConnectionName)

inputPost :: FormInput -> Either ([FieldEr MNE.MyNioFieldError]) AppConfig
inputPost fi = do
  case inputDatabaseConnectionPost fi of
    Left e' -> Left $ allErrors ++ e'
    Right dbconfig ->
      (first $ const allErrors)
        $
        ((liftM9 AppConfig) <$> a <*> b <*> c <*> d <*> e <*> f <*> (const $ pure dbconfig) <*> (const $ pure "") <*> (const $ pure ""))  fi

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
      a = fieldValue isPresent (show AppConfigSiteHeading :: String)
      b = fieldValue isPresent (show AppConfigSiteSubHeading :: String)
      c = fieldValue isPresent ("siteRedirectFrom" :: String, "siteRedirectTo" :: String)
      d = fieldValue isPresent (show AppConfigSiteContactEmail :: String)
      e = fieldValue isPresent (show AppConfigSiteSideBarHtml :: String)
      f = fieldValue isPresent (show AppConfigSiteExtraHeadHtml :: String)

-- myff :: (Show a, FieldGetter a, Show b, FieldGetter b)
--   => NioValidateField a
--   -> NioValidateField b
--   -> NioFormKey -> NioFormKey -> FormInput -> Either FieldEr [(a, b)]
-- myff nv nv2 k k2 fi = case (,) <$> myGetFieldArray nv k fi <*> myGetFieldArray nv2 k2 fi of
--   Right (x, x') -> pure $ Data.List.zip x x'
--   Left e -> Left e

-- myGetFieldArray :: (Show a, FieldGetter a)
--   => NioValidateField a
--   -> NioFormKey -> FormInput -> Either FieldEr [a]
-- myGetFieldArray nv k fi = sequence $ (\x -> myGetField nv x fi) <$> hmm 0
--   where
--   hmm i = case (Data.List.find ((==) (k ++ "-" ++ show i) . fst) fi) of
--     Just x -> [fst x] ++ hmm (i + 1)
--     Nothing -> []
