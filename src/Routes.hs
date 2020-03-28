{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Routes (
    RouteUrl(..)
  , PlaceHolderUrl
  , renderPlaceHolderUrl
  , PublicRoute
  , PublicUrl
  , renderPublicUrl
  , renderStrPublicUrl
              ) where

import Data.Text
import Data.String.Conversions

data RouteUrl a b =
                Dashboard
              | ViewPost a
              | ViewPage a a
              | ViewImage a
              | DownloadFile a
              | AdminSettings
              | CreateCommentOnPost
              | ListPost
              | AdminCreatePost
              | AdminEditPost a
              | AdminDeletePost a
              | ListImage
              | AdminCreateImage
              | AdminEditImage a
              | AdminDeleteImage a
              | ListFile
              | AdminCreateFile
              | AdminEditFile a
              | AdminDeleteFile a
              | AdminListSinglePage
              | AdminCreateSinglePage
              | AdminDeleteSinglePage a
              | Register
              | Home
              | Login

instance Show (RouteUrl String b) where
  show (Dashboard) =                      "Dashboard"
  show (ViewPost a) =                     a
  show (ViewPage a _) =                   a
  show (ViewImage a) =                   a
  show (DownloadFile a) =                   a
  show (AdminSettings) =                  "AdminSettings"
  show (CreateCommentOnPost) =            "CreateCommentOnPost"
  show (ListPost) =                       "Blog"
  show (AdminCreatePost) =                "AdminCreatePost"
  show (AdminEditPost a) =                "AdminEditPost" ++ a
  show (AdminDeletePost a) =              "AdminDeletePost" ++ a
  show (ListImage) =                       "Gallery"
  show (AdminCreateImage) =                "AdminCreateImage"
  show (AdminEditImage a) =                "AdminEditImage" ++ a
  show (AdminDeleteImage a) =              "AdminDeleteImage" ++ a
  show (ListFile) =                       "Files"
  show (AdminCreateFile) =                "AdminCreateFile"
  show (AdminEditFile a) =                "AdminEditFile" ++ a
  show (AdminDeleteFile a) =              "AdminDeleteFile" ++ a
  show (AdminListSinglePage) =            "AdminListSinglePage"
  show (AdminCreateSinglePage) =          "AdminCreateSinglePage"
  show (AdminDeleteSinglePage a) =        "AdminDeleteSinglePage" ++ a
  show (Register) =                       "Register"
  show (Home) =                           "Home"
  show (Login) =                          "Login"

data PublicUrl
data PlaceHolderUrl
data NormalizedUrl

type PublicRoute = RouteUrl Int PublicUrl

showfy :: Show a => a -> String
showfy = cs . dropAround (== '"') . cs . show

class Show a => RouteParam a b where
  f :: RouteUrl a b -> String -> String
  normalParam :: RouteUrl a b -> RouteUrl String NormalizedUrl
  normalParam (ViewPost x) = ViewPost $ showfy x
  normalParam (ViewPage x x') = ViewPage (showfy x) (showfy x')
  normalParam (ViewImage x) = ViewImage $ showfy x
  normalParam (DownloadFile x) = DownloadFile $ showfy x
  normalParam (CreateCommentOnPost) = CreateCommentOnPost
  normalParam (Dashboard) = Dashboard
  normalParam (AdminSettings) = AdminSettings
  normalParam (ListPost) = ListPost
  normalParam (AdminCreatePost) = AdminCreatePost
  normalParam (AdminEditPost x ) = AdminEditPost $ showfy x
  normalParam (AdminDeletePost x ) = AdminDeletePost $ showfy x
  normalParam (ListImage) = ListImage
  normalParam (AdminCreateImage) = AdminCreateImage
  normalParam (AdminEditImage x ) = AdminEditImage $ showfy x
  normalParam (AdminDeleteImage x ) = AdminDeleteImage $ showfy x
  normalParam (ListFile) = ListFile
  normalParam (AdminCreateFile) = AdminCreateFile
  normalParam (AdminEditFile x ) = AdminEditFile $ showfy x
  normalParam (AdminDeleteFile x ) = AdminDeleteFile $ showfy x
  normalParam (AdminListSinglePage) =AdminListSinglePage
  normalParam (AdminCreateSinglePage) = AdminCreateSinglePage
  normalParam (AdminDeleteSinglePage a) = AdminDeleteSinglePage $ showfy a
  normalParam (Register) = Register
  normalParam (Login) = Login
  normalParam (Home) = Home

instance RouteParam Int PublicUrl where
  f _ = id

instance RouteParam String PublicUrl where
  f _ = id

instance RouteParam String PlaceHolderUrl where
  f _ = (":" ++)

renderUrl :: RouteParam a b => RouteUrl a b -> String
renderUrl r = case normalParam  r of
  Home ->                    "/"
  ViewPost x ->              "/post/view/" ++ f r x
  ViewPage _ x ->            "/" ++ f r x
  ViewImage x ->             "/gallery/view/" ++ f r x
  DownloadFile x ->          "/files/" ++ f r x
  CreateCommentOnPost ->     "/post/comment/create/"
  Dashboard ->               "/"
  AdminSettings ->           "/admin/settings/"
  ListPost ->                "/post/list"
  AdminCreatePost ->         "/admin/post/create/"
  AdminDeletePost x ->       "/admin/post/delete/" ++ f r x
  AdminEditPost x ->         "/admin/post/edit/" ++ f r x
  ListImage ->               "/image/list"
  AdminCreateImage ->        "/admin/image/create/"
  AdminDeleteImage x ->      "/admin/image/delete/" ++ f r x
  AdminEditImage x ->        "/admin/image/edit/" ++ f r x
  ListFile ->               "/file/list"
  AdminCreateFile ->        "/admin/file/create/"
  AdminDeleteFile x ->      "/admin/file/delete/" ++ f r x
  AdminEditFile x ->        "/admin/file/edit/" ++ f r x
  AdminListSinglePage ->     "/admin/page/list/"
  AdminCreateSinglePage ->   "/admin/page/create/"
  AdminDeleteSinglePage x -> "/admin/page/delete/" ++ f r x
  Login ->                   "/xyzabcadminlogin"
  Register ->                "/xyzabcadminregister"

renderPublicUrl :: RouteUrl Int PublicUrl -> String
renderPublicUrl r = renderUrl r

renderStrPublicUrl :: RouteUrl String PublicUrl -> String
renderStrPublicUrl r = renderUrl r

renderPlaceHolderUrl :: RouteUrl String PlaceHolderUrl -> String
renderPlaceHolderUrl r = renderUrl r
