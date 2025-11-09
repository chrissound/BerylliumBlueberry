{-# OPTIONS -Wno-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Template.Comment (CommentWithPost(..), commentsView, commentViewExtraAdmin) where

import Control.Monad
import Data.String.Conversions
import Data.String
import Database.PostgreSQL.Simple.Time
import Data.Time.Format
import Lucid
import Data.Text (Text)

import Models.Comment as MC
import qualified Routes as R
import Common
import Template.Types
import AppTypes

data CommentWithPost = CommentWithPost {
    cwtComment :: Comment
  , cwtPostTitle :: Text
} deriving (Show)

commentsView :: SiteView -> [CommentWithPost] -> (CommentWithPost -> Html ()) -> Html ()
commentsView sv comments commentHook = do
  basicContent sv "Comments" $ do
    with table_ [class_ "commentsTable"] $ do
      with tr_ [class_ "header-row"] $ do
        th_ "Author"
        th_ "Comment"
        th_ "Post"
        th_ "Date"
        th_ "Approved"
        th_ "Actions"
      forM_ comments $ \cwp -> do
        with tr_ [class_ $ if MC.approved (cwtComment cwp) then "approved-comment" else "pending-comment"] $ do
          td_ $ fromString $ cs $ MC.authorAlias $ cwtComment cwp
          td_ $ fromString $ cs $ MC.commentBody $ cwtComment cwp
          td_ $ fromString $ cs $ cwtPostTitle cwp
          td_ $ fromString $ formatUTC $ MC.postCreated $ cwtComment cwp
          td_ $ fromString $ if MC.approved (cwtComment cwp) then "✓ Yes" else "✗ No"
          with td_ [class_ "actions"] $ do
            commentHook cwp
      p_ ""

commentViewExtraAdmin :: CommentWithPost -> Html ()
commentViewExtraAdmin cwp = do
  let cid = MC.commentId (cwtComment cwp)
  if not (MC.approved $ cwtComment cwp)
    then do
      buttonView (renderParamUrl R.AdminApproveComment cid) "" "Approve" InfoBlue ExtraSmall
      fromString " "
    else pure ()
  buttonConfirmView (renderParamUrl R.AdminDeleteComment cid) "" "Delete" AlertRed ExtraSmall
  fromString " "
