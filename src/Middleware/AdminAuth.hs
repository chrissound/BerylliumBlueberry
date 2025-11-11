{-# LANGUAGE OverloadedStrings #-}
module Middleware.AdminAuth where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Web.Cookie
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import AppConfig

-- Protected paths that require authentication
protectedPrefixes :: [BS.ByteString]
protectedPrefixes = ["/admin"]

-- Paths that should be accessible without authentication
skippedPaths :: [BS.ByteString]
skippedPaths = ["/xyzabcadminlogin", "/xyzabcadminregister"]

-- Prefixes that should always be skipped (static files, etc.)
skippedPrefixes :: [BS.ByteString]
skippedPrefixes = ["/static/", "/files/", "/data/uploads/", "/favicon"]

-- Main middleware factory - takes AppConfig and returns Middleware
adminAuthMiddleware :: AppConfig -> Middleware
adminAuthMiddleware appConfig app req sendResponse = do
    if requiresAuth (rawPathInfo req)
        then do
            let isAuth = isAuthorized appConfig req
            if isAuth
                then app req sendResponse
                else sendResponse unauthorizedResponse
        else app req sendResponse

-- Check if a path requires authentication
requiresAuth :: BS.ByteString -> Bool
requiresAuth path =
    any (`BS.isPrefixOf` path) protectedPrefixes &&
    not (shouldSkipPath path)

-- Check if a path should skip authentication
shouldSkipPath :: BS.ByteString -> Bool
shouldSkipPath path =
    path `elem` skippedPaths ||
    any (`BS.isPrefixOf` path) skippedPrefixes

-- Check if request is authorized by validating cookie password
isAuthorized :: AppConfig -> Request -> Bool
isAuthorized appConfig req =
    case lookup hCookie (requestHeaders req) of
        Nothing -> False
        Just cookieHeader ->
            let cookies = parseCookies cookieHeader
            in case lookup "admin_password" cookies of
                Nothing -> False
                Just cookiePass -> TE.decodeUtf8 cookiePass == adminPassword appConfig

-- Return 404 to hide admin routes from unauthorized users
unauthorizedResponse :: Response
unauthorizedResponse = responseLBS
    status404
    [(hContentType, "text/html; charset=utf-8")]
    "Not Found"
