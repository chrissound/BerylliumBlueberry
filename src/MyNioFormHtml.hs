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
import qualified MyNioFieldError

data NioFormHtml = NioFormHtml {
    nioForm :: NioForm MyNioFieldError.MyNioFieldError
  , action :: PublicRoute
  }

nioformHtml :: NioFormHtml -> Html ()
nioformHtml (NioFormHtml nf action'') = basicNioformHtml nf $ R.renderPublicUrl action''

nioformHtmlFields :: [NioFieldView MyNioFieldError.MyNioFieldError] -> Html ()
nioformHtmlFields nf = forM_ nf nioformHtmlField 
