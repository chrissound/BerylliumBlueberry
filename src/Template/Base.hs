{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Template.Base
    ( module Template.Base
    , module Template.Types
    ) where

import Control.Monad
import qualified Data.Text as T
import Data.String.Conversions
import Data.String
import Text.Pandoc
import Control.Monad.Except
import Control.Monad.Trans.State.Strict
import Lucid

import Models.Comment (Comment, authorAlias, commentBody, postCreated)
import qualified Models.Post as M
import Models.PostType as PostType
import qualified Models.User as M
import qualified Routes as R
import Template.Types

navItemA :: (Term arg1 arg2, Term arg2 result, With (arg1 -> arg2)) => T.Text -> arg1 -> result
navItemA u t = li_ $ (with a_ [class_ "nav-item blog-nav-item", href_ u ] $ t)


alertBox :: BootAlertType -> Html () -> Html ()
alertBox alertType alertVal =
    with div_ [class_ (T.concat ["alert alert-dismissable ", t])] $ do
        with button_ [type_ "button", class_ "close", data_ "dismiss" "alert", data_ "aria-hidden" "yes"] ("")
        alertVal
    where
      t = case alertType of
        BootAlertDanger -> "alert-danger"
        BootAlertWarn -> "alert-warning"
        BootAlertInfo -> "alert-info"
        BootAlertSuccess -> "alert-success"

notificationTypeToBootAlert :: SiteNotification -> BootAlertType
notificationTypeToBootAlert NotificationInfo = BootAlertInfo
notificationTypeToBootAlert NotificationError = BootAlertDanger

generateScript :: (String, Maybe String, Maybe String) -> Html ()
generateScript (a,b,c) = with (script_ "")
  $ [src_ $ cs a]
  ++ maybe [] (\b' -> [integrity_ $ cs b']) b
  ++ maybe [] (\c' -> [crossorigin_ $ cs c']) c

siteView :: SiteView -> Html () -> Html ()
siteView sv body =
  doctypehtml_ $ do
    head_ $ do
      title_ $ fromString $ cs ((sv_pageTitle sv) <> " - " <> (sv_blogName sv))
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      toHtmlRaw $ siteExtraHeadHtml sv
      link_ [href_ "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css", rel_ "stylesheet"]
      link_ [href_ "//maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css", rel_ "stylesheet"]
      link_ [href_ "/static/blog.css", rel_ "stylesheet"]
      link_ [href_ "//use.fontawesome.com/releases/v5.7.0/css/all.css", rel_ "stylesheet"]
      link_ [href_ "//use.fontawesome.com/releases/v5.7.0/css/fontawesome.css", rel_ "stylesheet"]
      link_ [href_ "//use.fontawesome.com/releases/v5.7.0/css/all.css", rel_ "stylesheet"]
      mconcat $ fmap (\s -> link_ [href_ $ cs s, rel_ "stylesheet"]) $ css sv
      mconcat $ fmap generateScript $ scripts sv
    body_ $ do
      with div_ [class_ "blog-masthead"] $
        with div_ [class_ "container"] $ do
          headerView sv
          with nav_ [class_ "nav nav-tabs"] $
            (navItemA (fromString $ R.renderPublicUrl R.ListPost) "Blog")
            <>
            (if (displayGallery sv) then
               mconcat [
                (navItemA (fromString $ R.renderPublicUrl R.ListImage) "Gallery")
              ]
            else pure ()
            )
            <>
            case (loggedInUser sv) of
              Just _ -> do
                (mconcat (
                  fmap (\x -> navItemA (fromString $ R.renderStrPublicUrl x) ( fromString $ show x))
                  [
                    R.AdminListSinglePage
                  , R.ListFile
                  , R.AdminSettings
                  ]
                  ))
              Nothing -> pure ()
            <>
                mconcat (
                  fmap (navItemA <$> (fromString . R.renderStrPublicUrl) <*> ( fromString . show))
                  (
                    (R.ViewPage <$> (cs . M.postTitle .  postV) <*> (cs . M.postEasyId .  postV))
                    <$> pages sv)
                  )
      with div_ [class_ "container"] $ do
        case (notification sv) of
          Just (n, nt) ->
            with div_ [class_ "row"] $ do
              with div_ [class_ "col-sm-6 col-md-offset-3"] $ do
                alertBox (notificationTypeToBootAlert nt) (fromString $ cs n)
            <> body
          Nothing -> body
      footerView

footerView :: Html ()
footerView = do
  with div_ [ class_ "blog-footer" ] $ return ()

panel' :: T.Text -> Html () -> Html ()
panel' title ct =
  with div_ [class_ "row"] $
    with div_ [class_ "col-sm-12"] $
      with div_ [ class_ "panel panel-info", style_ "margin-top: 30px;" ] $ do
        with div_ [ class_ "panel-heading" ] $
          with div_ [ class_ "panel-title" ] $ fromString $ cs title
        with div_ [ class_ "panel-body" ] $ 
          div_ ct

panelWithErrorView :: T.Text -> Maybe T.Text -> Html () -> Html ()
panelWithErrorView title mError ct =
  panel' title $ do
      case mError of
        Just errMsg -> alertBox BootAlertDanger (fromString $ cs errMsg)
        Nothing -> mempty
      div_ ct

pageViewExtraAdmin' :: Html ()
pageViewExtraAdmin' = do
  br_ []
  buttonView (R.renderPublicUrl R.AdminCreateSinglePage) "" "Create Page" InfoBlue Normal

pageViewExtraAdmin :: PagePost -> Html ()
pageViewExtraAdmin p = do
  buttonConfirmView  (renderParamUrl R.AdminDeleteSinglePage $ M.postId $ postV p) "" "Delete" AlertRed ExtraSmall
  fromString " "
  buttonView  (renderParamUrl R.AdminEditPost $ M.postId $ postV p) "" "Edit" InfoBlue ExtraSmall
  fromString " "


postViewExtraAdmin :: M.Post -> Html ()
postViewExtraAdmin p = do
  buttonConfirmView  (renderParamUrl R.AdminDeletePost $ M.postId p) "" "Delete" AlertRed ExtraSmall
  fromString " "
  buttonView (renderParamUrl R.AdminEditPost $ M.postId p) "" "Edit" InfoBlue ExtraSmall
  fromString " "


postViewExtraEmpty :: M.Post -> Html ()
postViewExtraEmpty = const $ return ()

constHtml :: a -> Html ()
constHtml = const $ return ()

postsViewExtraAdmin :: Html ()
postsViewExtraAdmin = do
  br_ []
  buttonView (R.renderPublicUrl R.AdminCreatePost) "" "Create Post" InfoBlue Normal


headerView :: SiteView -> Html ()
headerView sv =
  header_ $ do
    with div_ [ class_ "blog-header" ] $ do
      with div_ [ class_ "row" ] $ do
        with div_ [ class_ "col-sm-12" ] $ do
          img_ [src_ "/static/main-img.jpg", class_ "me-picture img-responsive center-block"]
          with div_ [class_ "clear"] $ pure ()
      with div_ [ class_ "row" ] $ do
        with div_ [ class_ "col-sm-12 my-header" ] $ do
          with h1_ [ class_ "header-title" ] $ fromString $ cs $ sv_blogName sv
          with p_ [ class_ "header-slogan" ] $ fromString $ cs $ sv_blogDesc sv
          case (loggedInUser sv) of
            Just u -> p_ $ fromString $ "Logged in as: " ++ (cs $ M.userName u)
            Nothing -> return ()
      with div_ [ class_ "row" ] $ do
        with div_ [ class_ "col-sm-12 header-info-container" ] $ do
          with div_ [ class_ "center-text-flex" ] $ do
            with p_ [ class_ "header-info" ] $ with a_ [href_ $ "mailto:" <> contactEmail sv] $
              (with i_ [class_ "fas fa-envelope"] $ pure ()) <> (fromString . cs) (" email: " <> contactEmail sv)

pagesView :: SiteView -> [PagePost] -> (PagePost -> Html ()) -> Html ()
pagesView sv posts hook = do
  basicContent sv "Pages" $ do
      forM_ posts $ \post ->
        div_ $ do
          let url = R.renderStrPublicUrl $ R.ViewPage (cs $ M.postEasyId $ postV post) (cs $ M.postEasyId $ postV post)
          hook post
          with a_ [ href_ (fromString $ url) ] $ fromString $ cs $ M.postTitle (postV post)
      p_ ""


mtHtml :: String -> Html ()
mtHtml md = do
  let f i = return $ evalState ((runStateT (runExceptT $ unPandocPure i)) def) def
  let m = readMarkdown (def { readerExtensions = pandocExtensions }) $ cs md :: PandocPure Pandoc
  (a, _) <- f m
  case a of
    (Right x) -> do
      (z, _) <- f $ writeHtml5String def x
      case z of
        Right x' -> toHtmlRaw x'
        Left _ -> span_ ""
    (Left _) -> span_ ""

commentView :: Models.Comment.Comment -> Html ()
commentView c = do
  div_ [ class_ "row" ] $ do
    div_ [ class_ "col-sm-8 blog-main" ] $ do
      if (authorAlias c /= "") then
        p_ [ class_ "" ] $ fromString $ cs $ authorAlias c
        else
        p_ [ class_ "" ] $ fromString $ "Anonymous"
      p_ $ fromString $ "Posted on: " <> (formatUTC $ postCreated c)
      p_ [ class_ "" ] $ fromString $ cs $ commentBody c
      hr_ []


pageView :: SiteView -> M.Post -> Html ()
pageView sv post = do
  div_ [ class_ "row" ] $ do
    div_ [ class_ "col-sm-8 blog-main" ] $ do
      h1_ [ class_ "blog-post-title" ] $ fromString $ cs $ M.postTitle post
      article_ $
        with div_ [ class_ "blog-post" ] $ do
          mtHtml $ cs $ M.postBody post
    sideBarView sv


loginView :: Html ()
loginView = do
  div_ [ class_ "blog-header" ] $ do
    h1_ "Login"
    p_ [ class_ "blog-description" ] $ "test"
