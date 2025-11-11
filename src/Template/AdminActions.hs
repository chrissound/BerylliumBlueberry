{-# LANGUAGE OverloadedStrings #-}

module Template.AdminActions
  ( renderPostActions
  , renderFileActions
  , renderImageActions
  , renderCreatePostButton
  , renderCreateFileButton
  , renderCreateImageButton
  ) where

import Data.String (fromString)
import Lucid

import qualified Models.Post as M
import qualified Models.File as MF
import qualified Models.Image as MI
import qualified Routes as R
import Template.Types
import ViewContext (ViewContext)
import qualified ViewContext as VC

-- | Render admin actions (delete/edit buttons) for a post
-- Only renders if the user is an admin
renderPostActions :: ViewContext -> M.Post -> Html ()
renderPostActions ctx p
  | VC.isAdmin ctx = do
      buttonConfirmView (renderParamUrl R.AdminDeletePost $ M.postId p) "" "Delete" AlertRed ExtraSmall
      fromString " "
      buttonView (renderParamUrl R.AdminEditPost $ M.postId p) "" "Edit" InfoBlue ExtraSmall
      fromString " "
  | otherwise = return ()

-- | Render admin actions (delete/edit buttons) for a file
-- Only renders if the user is an admin
renderFileActions :: ViewContext -> MF.File -> Html ()
renderFileActions ctx p
  | VC.isAdmin ctx = do
      buttonConfirmView (renderParamUrl R.AdminDeleteFile $ MF.fileId p) "" "Delete" AlertRed ExtraSmall
      fromString " "
      buttonView (renderParamUrl R.AdminEditFile $ MF.fileId p) "" "Edit" InfoBlue ExtraSmall
      fromString " "
  | otherwise = return ()

-- | Render admin actions (delete/edit buttons) for an image
-- Only renders if the user is an admin
renderImageActions :: ViewContext -> MI.Image -> Html ()
renderImageActions ctx p
  | VC.isAdmin ctx = do
      buttonConfirmView (renderParamUrl R.AdminDeleteImage $ MI.imageId p) "" "Delete" AlertRed ExtraSmall
      fromString " "
      buttonView (renderParamUrl R.AdminEditImage $ MI.imageId p) "" "Edit" InfoBlue ExtraSmall
      fromString " "
  | otherwise = return ()

-- | Render a "Create Post" button at the page level
-- Only renders if the user is an admin
renderCreatePostButton :: ViewContext -> Html ()
renderCreatePostButton ctx
  | VC.isAdmin ctx = do
      br_ []
      buttonView (R.renderPublicUrl R.AdminCreatePost) "" "Create Post" InfoBlue Normal
  | otherwise = return ()

-- | Render a "Create File" button at the page level
-- Only renders if the user is an admin
renderCreateFileButton :: ViewContext -> Html ()
renderCreateFileButton ctx
  | VC.isAdmin ctx = do
      br_ []
      buttonView (R.renderPublicUrl R.AdminCreateFile) "" "Create File" InfoBlue Normal
  | otherwise = return ()

-- | Render a "Create Image" button at the page level
-- Only renders if the user is an admin
renderCreateImageButton :: ViewContext -> Html ()
renderCreateImageButton ctx
  | VC.isAdmin ctx = do
      br_ []
      buttonView (R.renderPublicUrl R.AdminCreateImage) "" "Create Image" InfoBlue Normal
  | otherwise = return ()
