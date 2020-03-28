{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS -Wno-unused-imports #-}

module Server where

import Data.Binary
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Map.Strict as MS
import Data.Time.Clock
import Data.Time.Calendar
import User
import GHC.Generics
import qualified Data.Binary.Orphans
import Models.PostType
import Data.Maybe

type MapString a = Map String a
-- this holds a single session
type Session = SessionRecord (MapString String)
type AppStateSessions = MapString (Session)

data SessionRecord a = SessionRecord {lastModified:: UTCTime,  sessionV :: a} deriving (Show, Generic)
instance Binary Session

emptySessionRecord :: Session
emptySessionRecord = SessionRecord { sessionV = empty, lastModified = UTCTime (ModifiedJulianDay 0) 0}

instance UserSession AppStateSessions (Session) where
  addSession s sId t = do
    let newRecord = fromMaybe emptySessionRecord (session s sId)
    insert
      (sessionId sId)
      (newRecord { lastModified = t})
      s
  session s (SessionId x) = MS.lookup x s
  modifySessionRecord s (SessionId sId) f = do
    let sr = session s $ SessionId sId
    insert sId (f sr) s
  logout s sId = delete (sessionId sId) s

sessionId :: SessionId -> String
sessionId (SessionId x) = x

data AppState = AppState
  {
    sessions :: AppStateSessions
  , appStatePages :: [PagePost]
  } deriving (Show, Generic)

data AppStateSave = AppStateSave
  {
    sessions' :: AppStateSessions
  } deriving (Show, Generic)
instance Binary AppStateSave

instance Binary AppState where
  put (AppState s _ ) = put $ AppStateSave s
  get = fmap (flip AppState []) get

newtype ScottySessionT m a = ScottySessionT {
  runScottySessionT :: ReaderT (TVar AppState) m a
} deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Some helpers to make this feel more like a state monad.
gets :: (MonadIO m) => (AppState -> a) -> ScottySessionT m a
gets f = ask >>= liftIO . readTVarIO >>= return . f

getAppState :: (MonadIO m) => ScottySessionT m AppState
getAppState = gets id

modify :: (MonadIO m) => (AppState -> AppState) -> ScottySessionT m ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

clearOldSessions :: UTCTime -> AppState -> AppState
clearOldSessions expire  state = do
  state {sessions = MS.filter (((<) expire) . lastModified) (sessions state) }
