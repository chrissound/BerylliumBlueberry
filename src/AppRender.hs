{-# LANGUAGE OverloadedStrings #-}
module AppRender where

import Data.Text
import AppTypes
import Lucid

import Control.Monad.Reader
import Template.Base
import Web.Scotty.Trans
import Web.Scotty.Cookie
import Data.String.Conversions
import AppCommon
import ScottyInput
import Models.PostType
import Models.Image as Image
import AppConfig
import NioFormHtml
import NioFormTypes
import NioForm
import Database.PostgreSQL.Simple
import qualified MyNioFieldError as MNE
import qualified Data.Map.Strict as MS

renderPage :: Text -> Html () -> AppAction ()
renderPage t h = siteViewDataDef t >>= flip renderScottyHtmlSv h

withSvRenderPage :: Text -> (SiteView -> Html ()) -> AppAction ()
withSvRenderPage t h = siteViewDataDef t
  >>= (\sv -> renderScottyHtmlSv sv (h sv))

renderPage' :: Text -> MaybeNotification -> Html () -> AppAction ()
renderPage' t n h = siteViewData' t n >>= flip renderScottyHtmlSv h

withSvRenderPage' :: Text -> MaybeNotification -> (SiteView -> Html ()) -> AppAction ()
withSvRenderPage' t n h = siteViewData' t n
  >>= (\sv -> renderScottyHtmlSv sv (h sv))

renderScottyHtmlSv :: (ScottyError e, MonadIO m) => SiteView -> Html () -> ActionT e m ()
renderScottyHtmlSv sv = html . cs . renderText . (siteView sv)

formAction ::
  NioForm MNE.MyNioFieldError
  -> (FormInput -> Either [FieldEr MNE.MyNioFieldError] t)
  -> (NioForm MNE.MyNioFieldError -> Html ())
  -> Text
  -> Text
  -> (t -> AppAction ())
  -> AppAction ()
formAction f i htmlf  t ee a = do
  (scottyFormInput >>= pure . runInputForm f i) >>= \case
    Right x -> a x
    Left e -> renderPage t (panelWithErrorView t (Just  ee) $ htmlf e)

svd :: Text
        -> MaybeNotification
        -> AppAction SiteView
svd t n = do
  c <- liftAndCatchIO connection
  -- Check if admin by verifying cookie password
  appConfig <- liftIO getAppConfig
  cc <- getCookies
  let isAdminUser = case MS.lookup "admin_password" cc of
        Just cookiePass -> cs cookiePass == adminPassword appConfig
        Nothing -> False

  (\pp images -> SiteView
      (siteHeading appConfig)
      (siteSubHeading appConfig)
      t
      isAdminUser
      n
      pp
      ((Prelude.length (images :: [Image])) /= 0)
      (siteContactEmail appConfig)
      (siteSideBarHtml appConfig)
      (AppConfig.siteExtraHeadHtml appConfig)
      []
      []
    ) <$> liftIO getPagePosts <*> (liftAndCatchIO $ getAllImages c)
  where
    getAllImages conn = query_ conn "SELECT \"imageId\", \"imageTitle\", \"imageEasyId\", \"imageCreated\", \"imageFile\" FROM \"image\"" :: IO [Image]

siteViewDataDef :: Text -> AppAction SiteView
siteViewDataDef t = svd t Nothing

siteViewData' :: Text -> MaybeNotification -> AppAction SiteView
siteViewData' t n = svd t n
