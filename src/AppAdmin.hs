{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-do-bind #-}
module AppAdmin where 

import Data.Map as Map
import qualified Routes as R
import Web.Scotty.Trans
import Web.Cookie
import Web.Scotty.Cookie
import System.Entropy
import Control.Monad.Reader
import Blog

import Data.Time.Clock
import Data.Text (Text, map)
import Data.Bool
import Data.List.Utils (replace)

import Lucid

import Forms.Login
import Forms.Post2
import NioForm
import NioFormExtra

import Models.Post as Post
import Models.PostType as PostType
import Models.User as M
import User
import Database

import Data.ByteString.Base64 as B64
import Database.PostgreSQL.ORM.Model
import Template.Base

import ScottyInput

import AppRender
import AppCommon
import AppPost
import AppAdminSinglePage
import AdminSettings
import AppAdminGalleryImage
import AppAdminFile
import AppPostTypeI

adminServer :: AppServer ()
adminServer = do
  -- get (webRoute R.Register) $ do
  --   registerAction
  get (webRoute R.Login) $ do
    loginAction
  post (webRoute R.Login) $ do
    loginAction'
  get (webRoute R.AdminCreatePost) $ do
    verifyAuth
    viewCreatePostAction
  post (webRoute R.AdminCreatePost) $ do
    verifyAuth
    createPostAction
  get (webRoute $ R.AdminEditPost "postId") $ do
    verifyAuth
    param "postId" >>= editPostAction'
  post (webRoute $ R.AdminEditPost "postId") $ do
    verifyAuth
    param "postId" >>= editPostAction
  getPost (webRoute $ R.AdminDeletePost "postId") $ do
    verifyAuth
    param "postId" >>= deletePostTypeActionByPostId
    param "postId" >>= deletePostAction
  adminSinglePage
  adminSettings
  adminGallery
  adminFile

loginAction :: AppAction ()
loginAction = do
  let t = "Login"
  renderPage t (panelWithErrorView t Nothing $ loginFormLucid' loginForm')

loginAction' :: AppAction ()
loginAction' = do
  let t = "Login"
  formAction loginForm' inputLogin' loginFormLucid' t "Failed to login" $ \(LoginRequest a b) -> do
    loginRes <- liftAndCatchIO connection >>= \c -> liftAndCatchIO $ authUser c (UserName a) (UserPassword $ b)
    if | loginRes == True -> do
          randBytes <- liftAndCatchIO $ getEntropy 512
          let sId@(SessionId sId') = SessionId $ cs $ B64.encode $ randBytes
          setCookie ( defaultSetCookie { setCookieName = "session_id", setCookieValue = cs sId'})
          setCookie ( defaultSetCookie { setCookieName = "nocache", setCookieValue = "true"})
          nowTime <- liftAndCatchIO getCurrentTime
          lift $ withAppSessions $ (\s -> addSession s sId nowTime)
          lift $ withSession sId $ loginSession $ cs $ a
          redirect $ cs $ R.renderPublicUrl R.Dashboard
        | otherwise -> renderPage t (panelWithErrorView t (Just "Failed to login") $ loginFormLucid' loginForm')


viewCreatePostAction :: AppAction ()
viewCreatePostAction = do
  let t = "Create Post"
  renderPage' t Nothing (panelWithErrorView t Nothing  $ Forms.Post2.postFormLucid (Forms.Post2.postForm))

createPostAction :: AppAction ()
createPostAction = do
  let panel m v = panelWithErrorView "Create Post" (m) $ Forms.Post2.postFormLucid v
  ct <- liftIO $ getCurrentTime
  initialInput <- scottyInput
  eid <- maybe "" (id) <$> (pure $ Map.lookup "postTitle" $ Map.fromList initialInput)
  let formInput =
          Map.insert "approved" "0"
        $ Map.insert "postCreated" (show ct)
        $ Map.adjust
            (  replace " " "-"
             . (\x -> bool x eid (x == ""))
            )
            "postEasyId"
        $ Map.fromList initialInput
  case runInputForm Forms.Post2.postForm inputPost $ toList formInput of
    Right p -> do
      c <- liftAndCatchIO connection
      (liftAndCatchIO $ trySave c (preProcessPost p)) >>= \case
        Right x ->
          (
              liftAndCatchIO
              $ trySave c
              $ PostType NullKey (DBRef $ fromIntegral . idInteger $ Post.postId x) (fromEnum PostTypeBlog)
          ) >>= \case
            Right _ -> redirect $ cs $ R.renderPublicUrl R.ListPost
            Left e ->  renderScottyHtml $ panel (Just $ cs $ show e) (Forms.Post2.postForm)
        Left e -> renderScottyHtml $ panel (Just $ cs $ show e) (Forms.Post2.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      renderPage' ("Create Post") (Just ("Error submitting comment", NotificationError)) (extra)

editPostAction :: Int -> AppAction ()
editPostAction x = do
  let panel m v = panelWithErrorView "Edit Post" (m) $ Forms.Post2.postEditFormLucid x v
  formInput <- scottyFormInput
  let f = runInputForm Forms.Post2.postForm inputPost $ formInput
  case f of
    Right p -> AppAdmin.processPost p ((flip panel) Forms.Post2.postForm)
    Left nferr -> do
      let extra = panel Nothing $ nferr
      renderPage' ("Edit Post") (Just ("Error submitting comment", NotificationError)) (extra)

editPostAction' :: Int -> AppAction ()
editPostAction' x = do
  p <- getPostElseError x
  renderPage'
    "Edit Post"
    Nothing
    (
      panelWithErrorView "Edit Post" Nothing
      $ Forms.Post2.postEditFormLucid
      (idInteger $ Post.postId p)
      (postForm' p)
    )

deletePostAction :: Int -> AppAction ()
deletePostAction x = do
  c <- liftAndCatchIO connection
  p <- liftAndCatchIO $ findRow c (DBRef $ fromIntegral x)
  case p of
    Just p' -> do
      d <- liftAndCatchIO $ destroy c (p' :: Post)
      case d of
        Right _ -> redirect $ cs $ R.renderPublicUrl R.ListPost
        Left e -> error $ "Validation error:" ++ show e
    Nothing -> error $ "Post not found: " ++ show p

processPost :: Post -> (Maybe Text -> Html ()) -> AppAction ()
processPost p panel = do
  c <- liftAndCatchIO connection
  (liftAndCatchIO $ trySave c (preProcessPost p)) >>= \case
    Right _ -> redirect $ cs $ R.renderPublicUrl R.ListPost
    Left e -> renderScottyHtml $ panel (Just $ cs $ show e)

preProcessPost :: Post -> Post 
preProcessPost p = p { postEasyId = Data.Text.map (\x -> bool '-' x (x /= ' ')) $ postEasyId p}
