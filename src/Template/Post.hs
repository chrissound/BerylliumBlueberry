{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Template.Post where

import Control.Monad
import Data.String.Conversions
import Data.String
import Lucid
import Data.List
import Data.Function
import Data.Time.Clock
import Data.Time.Calendar
import Database.PostgreSQL.Simple.Time

import qualified Models.Post as M
import qualified Routes as R

import Template.Types
import Template.Base

fff :: UTCTimestamp -> UTCTime
fff (Finite x) = x
fff _ = error "Not fininte???"

gpby :: [M.Post] -> [(Int, [M.Post])]
gpby = (f) . (groupBy ((==) `on` getYearFromPost )) where
  f (x:xs) = [(getYearFromPost $ head x, x)] ++ f xs
  f [] = []
  z = (\(year,_,_) -> fromIntegral year) . toGregorian
  getYearFromPost = z . utctDay . fff . M.postCreated

postsView :: SiteView -> [M.Post] -> (M.Post -> Html ()) -> String -> Html ()
postsView sv posts postHook t = do
  basicContent sv t $ do
    with table_ [class_ "postsTable"] $ do
      forM_ (gpby posts) $ \(year, posts') -> do
        tr_ $ do
          td_ $ pure ()
          td_ $ h3_ (fromString $ show year)
          td_ $ pure ()
        forM_ posts' $ \post ->
          tr_ $ do
            with td_ [class_ "date"] $ do
              postHook post
              span_ (fromString $ formatUTC $ M.postCreated post) -- <> " "
            with td_ [class_ "postTitle"] $ do
              with a_ [ href_ (fromString $ R.renderStrPublicUrl $ R.ViewPost $ cs $ M.postEasyId post) ] $ fromString $ cs $ M.postTitle post
            td_ $ mconcat $ intersperse (span_ " ") $
              (fmap (\x -> with a_ [href_ $ cs $ R.renderStrPublicUrl $ R.TaggedPost $ cs $ x, class_ "badge badge-secondary"] $ fromString $ cs x) $ M.getPostTags  $ M.postTags post)
        p_ ""

postView :: SiteView -> M.Post -> Html () -> Html ()
postView sv post commentHtml = do
  div_ [ class_ "row" ] $ do
    div_ [ class_ "col-sm-8 blog-main" ] $ do
      h1_ [ class_ "blog-post-title" ] $ fromString $ cs $ M.postTitle post
      article_ $
        with div_ [ class_ "blog-post" ] $ do
          p_ $ fromString $ "Posted on: " <> (formatUTC $ M.postCreated post)
          mtHtml $ cs $ M.postBody post
          div_ $ do
            commentHtml
    sideBarView sv
