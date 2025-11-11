{-# LANGUAGE OverloadedStrings #-}

module ViewContext
  ( ViewContext(..)
  , fromSiteView
  ) where

import qualified Template.Types as TT

-- | Context for rendering views, encapsulating authentication state
data ViewContext = ViewContext
  { isAdmin :: Bool
  } deriving (Show, Eq)

-- | Create a ViewContext from a SiteView
-- Extracts the isAdmin field from SiteView
fromSiteView :: TT.SiteView -> ViewContext
fromSiteView sv = ViewContext { isAdmin = TT.isAdmin sv }
