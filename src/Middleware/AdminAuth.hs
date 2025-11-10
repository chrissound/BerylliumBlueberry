{-# LANGUAGE OverloadedStrings #-}
module Middleware.AdminAuth where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Web.Cookie
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String.Conversions
import qualified Data.Map.Strict as MS
import Control.Concurrent.STM
import Server

-- Protected paths that require authentication
protectedPrefixes :: [BS.ByteString]
protectedPrefixes = ["/admin"]

-- Paths that should be accessible without authentication
skippedPaths :: [BS.ByteString]
skippedPaths = ["/xyzabcadminlogin", "/xyzabcadminregister"]

-- Prefixes that should always be skipped (static files, etc.)
skippedPrefixes :: [BS.ByteString]
skippedPrefixes = ["/static/", "/files/", "/data/uploads/", "/favicon"]

-- Main middleware factory - takes TVar AppState and returns Middleware
adminAuthMiddleware :: TVar AppState -> Middleware
adminAuthMiddleware appStateVar app req sendResponse = do
    if requiresAuth (rawPathInfo req)
        then do
            isAuth <- isAuthorized appStateVar req
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

-- Check if request is authorized by validating session
isAuthorized :: TVar AppState -> Request -> IO Bool
isAuthorized appStateVar req = do
    case lookup hCookie (requestHeaders req) of
        Nothing -> return False
        Just cookieHeader -> do
            let cookies = parseCookies cookieHeader
            case lookup "session_id" cookies of
                Nothing -> return False
                Just sId -> checkSession appStateVar (cs sId)

-- Verify session has username (indicating logged in admin)
checkSession :: TVar AppState -> String -> IO Bool
checkSession appStateVar sId = do
    appState <- readTVarIO appStateVar
    case MS.lookup sId (sessions appState) of
        Nothing -> return False
        Just session -> return $ MS.member "username" (sessionV session)

-- Return 404 to hide admin routes from unauthorized users
unauthorizedResponse :: Response
unauthorizedResponse = responseLBS
    status404
    [(hContentType, "text/html; charset=utf-8")]
    "Not Found"
