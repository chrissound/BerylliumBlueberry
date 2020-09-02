{-# LANGUAGE OverloadedStrings #-}
module AppRender where

import Data.Text
import AppTypes
import Lucid

import Control.Monad.Reader
import Template.Base
import Web.Scotty.Trans
import Data.String.Conversions
import AppCommon
import ScottyInput
import Models.PostType
import Models.Image as Image
import AppConfig
import NioFormHtml
import NioFormTypes
import NioForm
import Database.PostgreSQL.ORM

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
  NioForm
  -> (FormInput -> Either [FieldEr] t)
  -> (NioForm -> Html ())
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
  (\u pp appConfig images -> SiteView
      (siteHeading appConfig)
      (siteSubHeading appConfig)
      t
      u
      n
      pp
      ((Prelude.length (images :: [Image])) /= 0)
      (siteContactEmail appConfig)
      (siteSideBarHtml appConfig)
      (AppConfig.siteExtraHeadHtml appConfig)
      []
      []
    ) <$> getLoggedInUser <*> liftIO getPagePosts <*> liftIO getAppConfig <*> (liftAndCatchIO $ findAll c)

siteViewDataDef :: Text -> AppAction SiteView
siteViewDataDef t = svd t Nothing

siteViewData' :: Text -> MaybeNotification -> AppAction SiteView
siteViewData' t n = svd t n
