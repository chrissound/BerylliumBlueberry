{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Template.Types where

import Control.Monad
import qualified Data.Text as T
import Data.String.Conversions
import Data.String
import Database.PostgreSQL.ORM (DBKey)
import Database.PostgreSQL.Simple.Time
import Data.Time.Format
import Text.Pandoc
import Control.Monad.Except
import Control.Monad.Trans.State.Strict
import Lucid

import qualified Models.User as M
import qualified Models.Post as M
import Models.PostType as PostType
import qualified Routes as R
import Common

data SiteNotification = NotificationInfo | NotificationError
type MaybeNotification = Maybe (T.Text, SiteNotification)

data SiteView = SiteView
  { sv_blogName :: T.Text
  , sv_blogDesc :: T.Text
  , sv_pageTitle :: T.Text
  , loggedInUser :: Maybe M.User
  , notification :: MaybeNotification --, sv_user :: Maybe
  , pages :: [PagePost]
  , displayGallery :: Bool
  , contactEmail :: T.Text
  , sideBarHtml :: T.Text
  , siteExtraHeadHtml :: T.Text
  , scripts :: [(String, Maybe String, Maybe String)]
  , css :: [String]
  }

data ButtonSize = Normal | ExtraSmall

data ButtonType = PriBlue | InfoBlue | AlertRed

data BootAlertType
   = BootAlertDanger
   | BootAlertWarn
   | BootAlertInfo
   | BootAlertSuccess

renderParamUrl :: IsString a => (Int -> R.RouteUrl Int R.PublicUrl) -> DBKey -> a
renderParamUrl r k = fromString $ R.renderPublicUrl $ r $ idInteger $ k

buttonConfirmView :: String -> String -> String -> ButtonType -> ButtonSize -> Html ()
buttonConfirmView l c t bt bs = with a_
  [ href_ $ cs l
  , class_ . fromString $ implode ["btn", c, btClass, bsClass]
  , onclick_ "return confirm('Delete post?');"
  ] $ fromString t
  where
    btClass = case bt of
      PriBlue -> "btn-primary"
      InfoBlue -> "btn-info"
      AlertRed -> "btn-danger"
    bsClass  = case bs of
      Normal -> ""
      ExtraSmall -> "btn-xs"

buttonView :: String -> String -> String -> ButtonType -> ButtonSize -> Html ()
buttonView l c t bt bs = with a_
  [ href_ $ cs l
  , class_ . fromString $ implode ["btn", c, btClass, bsClass]
  ] $ fromString t
  where
    btClass = case bt of
      PriBlue -> "btn-primary"
      InfoBlue -> "btn-info"
      AlertRed -> "btn-danger"
    bsClass  = case bs of
      Normal -> ""
      ExtraSmall -> "btn-xs"

implode :: [String] -> String
implode = concat . fmap (\x -> " " ++ x ++ " ")

sideBarView :: SiteView -> Html ()
sideBarView sv =
    div_ [ class_ "col-sm-3 col-sm-offset-1 blog-sidebar" ]
    $ div_ [ class_ "sidebar-module sidebar-module-inset" ]
      $ toHtmlRaw $ sideBarHtml sv

basicContent :: SiteView -> String -> Html ()  -> Html ()
basicContent sv t x =
  with div_ [ class_ "row" ] $ do
    with div_ [ class_ "col-sm-8 blog-main" ] $ do
      h1_ $ fromString t
      x
    sideBarView sv

formatUTC :: UTCTimestamp -> String
formatUTC (Finite x) = formatTime defaultTimeLocale "%d/%m/%Y" x
formatUTC _ = "???"
