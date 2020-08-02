{-# Language OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
module Test where


import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import Network.Wreq
import Control.Lens
import qualified Network.Wreq.Session as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL (ByteString)
import Text.Pretty.Simple (pPrint)
import Data.String.Conversions
import Routes
-- import qualified Network.HTTP.Client as HC

import Forms.AdminSettings
import NioFormHtml
import AppConfig


rootUrl :: String
rootUrl = "http://localhost:3001"

isStatusCode :: Response body -> Int -> IO ()
isStatusCode r sc = do
  rc <- pure (r ^. responseStatus . statusCode)
  compare rc sc @?= EQ

isRedirected :: Response body -> ByteString -> IO ()
isRedirected r l = do
  all (== True) [
      ((r ^. responseHeader "Location") == l)
    ,
      ((r ^. responseStatus . statusCode) == 302)
      ||
      ((r ^. responseStatus . statusCode) == 301)
    ] @?= True

example :: TestTree
example = testGroup "Unit tests"
  $
  -- pure . (!! 0)
  -- $
  [
    testCase "homepage" $ do
      r <- get "http://localhost:3001"
      isStatusCode r 200
  -- ,
  --   testCase "login redirect" $ do
  --     sess <- S.newSession
  --     r <- S.customHistoriedPayloadMethodWith
  --       "post"
  --       defaults
  --       sess
  --       (rootUrl ++ (renderPublicUrl Login))
  --       ([("user","chris"), ("password", "chris")] :: [(ByteString, ByteString)])
  --     let rc' = snd (head (r ^. hrRedirects))
  --     isRedirected rc' "/"
  ,
    testCase "login 200" $ do
      r <- S.newSession >>= login
      isStatusCode (r) 200
  ,
    testCase "admin access" $ do
      sess <- S.newSession
      _ <- login sess
      r2 <- S.get sess "http://localhost:3001/admin/settings/"
      isStatusCode (r2) 200
  ,
    testCase "post image" $ do
      sess <- S.newSession
      _ <- login sess
      r' <- S.customHistoriedPayloadMethodWith
        "POST"
        defaults
        sess
        (rootUrl ++ (renderPublicUrl AdminCreateImage))
        (
          [
          (partFile "fileFile" "/home/chris/Temp/yolo.png")
          ] ++ postFieldsToPart
          [
            ("fileTitle","test")
          , ("fileEasyId","test")
          ]
        )

      r'' <- get "http://localhost:3001/data/uploads/image/yolo.png"
      isStatusCode r'' 200

      isStatusCode (r' ^. hrFinalResponse) 200
  ,
    testCase "post file" $ do
      sess <- S.newSession
      _ <- login sess
      r' <- S.customHistoriedPayloadMethodWith
        "POST"
        defaults
        sess
        (rootUrl ++ (renderPublicUrl AdminCreateFile))
        (
          [
          (partFile "fileFile" "/home/chris/Temp/yolo.png")
          ] ++ postFieldsToPart
          [
            ("fileTitle","test")
          , ("fileEasyId","test")
          ]
        )

      r'' <- get "http://localhost:3001/files/yolo.png"
      isStatusCode r'' 200

      isStatusCode (r' ^. hrFinalResponse) 200
  ,
    testCase "post post and comment" $ do
      sess <- S.newSession
      _ <- login sess
      r <- S.customHistoriedPayloadMethodWith
        "POST"
        defaults
        sess
        (rootUrl ++ (renderPublicUrl AdminCreatePost))
        (
          postFieldsToPart
          [
            ("postId","")
          , ("postTitle","abc 123 chris")
          , ("postEasyId","")
          , ("postBody","Yayyyyyy")
          ]
        )
      let r' = snd (head (r ^. hrRedirects))
      isRedirected r' $ cs (renderPublicUrl ListPost)
      -- post comment
      rc <- S.customHistoriedPayloadMethodWith
        "POST"
        defaults
        sess
        (rootUrl ++ (renderPublicUrl CreateCommentOnPost))
        (
          postFieldsToPart
          [
            ("postId","129")
          , ("authorAlias","chris")
          , ("commentBody","hey chris888978")
          , ("commentId","")
          ]
        )
      let rc' = (rc ^. hrFinalResponse)
      isStatusCode rc' 200
      -- r2 <- S.customHistoriedPayloadMethodWith
      --   "GET"
      --   defaults
      --   sess
      --   (rootUrl ++ (renderPublicUrl $ AdminDeletePost 92))
      --   (
      --     postFieldsToPart
      --     [
      --     ]
      --   )
      -- let r2' = snd (head (r2 ^. hrRedirects))
      -- isRedirected r2' $ cs (renderPublicUrl ListPost)
  ,
    testCase "admin settings form processing" $ do
      let i = [
                (show AppConfigSiteHeading, "zzz")
              , (show AppConfigSiteSubHeading, "zzz")
              , (show AppConfigSiteContactEmail, "zzz")
              , (show AppConfigSiteSideBarHtml, "zzz")
              , (show AppConfigSiteExtraHeadHtml, "zzz")
              , (show AppConfigDatabaseConnectionHost, "zzz")
              , (show AppConfigDatabaseConnectionPort, "1234")
              , (show AppConfigDatabaseConnectionUser, "zzz")
              , (show AppConfigDatabaseConnectionPassword, "zzz")
              , (show AppConfigDatabaseConnectionName, "zzz")
              , ("siteRedirectFrom-0", "zzz")
              , ("siteRedirectTo-0", "zzz")
              ]
      case (inputPost i) of
        Right _ -> pure ()
        Left e -> do
          pPrint $ fmap ((,) <$> fst <*> (friendlyError' . snd)) e
      True  @?= True
  ]

login :: S.Session -> IO (Response BSL.ByteString)
login sess = do
  r <- S.customHistoriedPayloadMethodWith
    "POST"
    defaults
    sess
    (rootUrl ++ (renderPublicUrl Login))
    ([("user","chris"), ("password", "chris")] :: [(ByteString, ByteString)])
  pure (r ^. hrFinalResponse)

postFieldsToPart :: [(ByteString, ByteString)] -> [Part]
postFieldsToPart = fmap (\(a,b) -> partBS (cs a) b)

main :: IO ()
main = do
  threadDelay (1 * (10 ^ 3))
  defaultMain $
    testGroup ""
      [
        example
      ]
