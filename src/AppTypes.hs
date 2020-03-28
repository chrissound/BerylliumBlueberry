{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module AppTypes where

import qualified Data.Text.Lazy as L
import Web.Scotty.Trans
import Server
-- import AppModel

-- import Web.Scotty.Internal.Types
-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.State.Lazy as StateL

type App = ScottySessionT IO
type AppServer a = ScottyT L.Text App a
type AppAction a = ActionT L.Text App a
type AppActionT = ActionT L.Text App

data FileUpload = FileUpload deriving Show
data ImageResizedFileUpload = ImageResizedFileUpload deriving Show
newtype FileUploadName = FileUploadName String deriving Show
-- data FileUploadImage = FileUploadImage deriving Show

-- wtf :: a -> ScottyT L.Text App a
-- wtf = pure

-- wtf' :: Monad m => a -> MaybeT m a
-- wtf' = pure

-- bindd :: Monad m => m a -> (a -> m b) -> m b
-- bindd = (>>=)

-- wtf'' :: Monad m => m a -> MaybeT m a
-- wtf'' ma = ma >>= pure

-- fuck :: Monad m => ScottyT L.Text m Int
-- fuck = pure $ 5

-- -- fuck2 :: ScottyT L.Text IO String
-- fuck2 :: State (ScottyState L.Text IO) String
-- -- fuck2 = pure ""
-- fuck2 = do
--   x <- StateL.gets handler
--   pure ""

-- instance MonadTrans (ScottyT L.Text) where
--   -- lift  = _undefined :: m a -> ScottyT L.Text m a
--   lift x = ScottyT $ (fmap (_undefined) x) where
    -- z = do
    --   sss <- StateL.get
    --   state (f)
      -- lift _undefined :: State (ScottyState L.Text m) a



-- instance (MonadIO m) => MonadIO (ScottyT L.Text m) where
--   liftIO = lift . liftIO
