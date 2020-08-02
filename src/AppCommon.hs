{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module AppCommon (
    module AppCommon
  , module AppTypes
  -- , module Forms.Post
  , module Database
  , module Web.Scotty.Trans
  , module Data.Time.Clock
  , module User
  , module Common
  , module Data.String.Conversions
  , module Text.Pretty.Simple
                 ) where
import Text.Pretty.Simple
import Network.HTTP.Types
import Data.Time.Clock
import Server
import qualified Routes as R
import Data.Map.Strict as MS
import Control.Monad.Reader
import User hiding (session)
import Lucid
import Data.Bool
import Data.String
import Web.Scotty.Cookie
import Data.String.Conversions
import Web.Scotty.Trans
-- import Network.Wai.Parse

-- import Forms.Post

import AppTypes
import Common

import Database
import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple

import Models.User as M

webRoute :: R.RouteUrl String R.PlaceHolderUrl -> RoutePattern
webRoute = fromString . R.renderPlaceHolderUrl

getPost :: (MonadIO m, ScottyError e) => RoutePattern -> ActionT e m () -> ScottyT e m ()
getPost r a = do
  get r a
  post r a

verifyAuth :: AppAction ()
verifyAuth = do
  s <- AppCommon.session
  case s of
    Just s' -> bool (next) (pure ()) (member "username" $ sessionV s')
    Nothing -> next

session :: AppAction (Maybe Session)
session = do
  cc <- getCookies
  case MS.lookup "session_id" cc of
    Just sId -> do
      as <- lift $ getAppState
      return $ MS.lookup (cs sId) (sessions as)
    Nothing -> return Nothing

withSession :: SessionId -> (Maybe Session -> Session) -> App ()
withSession sId f = modify (\appState -> appState { sessions = modifySessionRecord (sessions appState) sId f})

withAppSessions :: (AppStateSessions -> AppStateSessions) -> App ()
withAppSessions f = modify (\appState -> appState { sessions = f (sessions appState)})

loginSession :: String -> Maybe Session -> Session
loginSession u (Just sr) = sr { sessionV = (insert "username" u $ sessionV sr)}
loginSession _ _ = error "should never happen... Forgot to call addSession?"


redirectRoute :: R.PublicRoute -> AppAction ()
redirectRoute r = redirect $ cs $ R.renderPublicUrl r

-- renderScottyHtmlSv :: (ScottyError e, MonadIO m) => SiteView -> Html () -> ActionT e m ()
-- renderScottyHtmlSv sv = do
--   html . cs . renderText . (siteView sv)

renderScottyHtml :: Html () -> AppAction ()
renderScottyHtml h = html . cs . renderText $ h

getLoggedInUser :: AppAction (Maybe M.User)
getLoggedInUser = do
  s <- AppCommon.session
  case s of
    Just s' -> do
      username <- return $ MS.lookup "username" (sessionV s')
      c <- liftAndCatchIO connection
      z <- liftAndCatchIO $ dbSelect c $ (addWhere (fromString $ cs $ sqlWhereEquals "userName") (Only username) modelDBSelect )
      case z of
        (r:_) -> return $ Just r
        (_) -> return Nothing
    Nothing -> return Nothing


todoMaybeQuickError :: Monad m => Maybe a -> m a
todoMaybeQuickError (Just x) = pure x
todoMaybeQuickError Nothing = error "???"

maybe404 :: (a -> AppAction ()) -> Maybe a -> AppAction ()
maybe404 = maybe (status status404)

scottyInput :: AppAction [(String, String)]
scottyInput = do
  x <- fmap (\(a,b) -> (cs a, cs b)) <$> params
  y <- (fmap (\(y',_) -> (cs y', cs y'))) <$> files
  -- appIO $ print "hey day ya"
  -- appIO $ print "hey day ya"
  -- appIO $ print (x ++ y)
  pure (x ++ y)

appIO :: IO a -> AppAction a
appIO = liftIO >>= pure

-- appServerApp :: IO a -> AppServer a
-- appServerApp = liftIO >>= pure
