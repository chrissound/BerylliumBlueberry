{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module MyNioFormHtml where

import Lucid
import NioFormTypes
import NioFormInstances
import NioFormExtra
import NioFormHtml
import Control.Monad
import Routes as R

data NioFormHtml = NioFormHtml {
    nioForm :: NioForm
  , action :: PublicRoute
  }

nioformHtml :: NioFormHtml -> Html ()
nioformHtml (NioFormHtml nf action'') = basicNioformHtml nf $ R.renderPublicUrl action''

nioformHtmlFields :: [NioFieldView] -> Html ()
nioformHtmlFields nf = forM_ nf nioformHtmlField 
