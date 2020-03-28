{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Database.PostgreSQL.ORM
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Migrations
-- import Database.PostgreSQL.ORM.Model

import Control.Exception
import Models.Comment as Comment
import Models.Post as Post
import Models.PostType as PostType
import Data.Int
import Control.Monad.Reader
import Data.String
import qualified Data.List as D.List (intersperse)
--import Text.Pretty.Simple (pPrint)
import Control.Concurrent.STM
import Web.Scotty.Trans
import Control.Concurrent
import Data.Time.Clock
import System.Environment
import System.Exit
import Data.Binary
import Network.Wai.Middleware.RequestLogger

-- Local package
import Server
import App
import Database

commentPostRefinfo :: DBRefInfo Comment Post
commentPostRefinfo = defaultDBRefInfo

migrateAll :: IO ()
migrateAll = do
  c <- connection
  up c
  up2 c
  up3 c
  up4 c
  up5 c

up :: Connection -> IO ()
up c = do
  migrate ( do
    create_table "post"
      [ column "postId" "serial PRIMARY KEY"
      , column "postTitle" "text NOT NULL"
      , column "postBody" "text NOT NULL"
      , column "postCreated" "timestamptz NOT NULL"
      , column "postEasyId" "text NOT NULL"
      ]
    ) c
  migrate ( do
    create_table "comment"
      [ column "commentId" "serial PRIMARY KEY"
      , column "postId" "integer references post"
      , column "commentBody" "text NOT NULL"]
    ) c
  migrate ( do
    create_table "user"
      [ column "userId" "serial PRIMARY KEY"
      , column "userName" "text NOT NULL"
      , column "userEmail" "text NOT NULL"
      , column "userPassword" "text NOT NULL"
      , column "userVerified" "boolean"]
    ) c

up2 :: Connection -> IO ()
up2 c = do
  migrate (add_column "comment" "approved" "boolean") c
  migrate (add_column "comment" "postCreated" "timestamptz NOT NULL") c
  migrate (add_column "comment" "authorAlias" "text") c

up3 :: Connection -> IO ()
up3 c = do
  migrate ( do
    create_table "postType"
      [ column "postTypeId" "serial PRIMARY KEY"
      , column "postId" "integer references post"
      , column "postType" "integer NOT NULL"
      ]
    ) c
  -- x <- executeMany c "INSERT into postType VALUES (bbbbb)" $ 
  z <- dbSelect c (modelDBSelect :: DBSelect Post)
  mapM_ (\p -> do
            s <- trySave c (PostType (NullKey) (mkDBRef p) (fromEnum $ PostTypeBlog))
            case s of
              Right _ -> pure ()
              Left x -> error $ show x
           ) z
  -- x <- executeMany c
  --   "insert into \"postType\" (\"postId\", \"postType\") values (?,?)"
  --   [(1 :: Int,1 :: Int)]
  -- print x

up4 :: Connection -> IO ()
up4 c = do
  migrate ( do
    create_table "image"
      [ column "imageId" "serial PRIMARY KEY"
      , column "imageTitle" "text NOT NULL"
      , column "imageEasyId" "text NOT NULL"
      , column "imageCreated" "timestamptz NOT NULL"
      , column "imageFile" "text NOT NULL"
      ]
    ) c

up5 :: Connection -> IO ()
up5 _ = do
  pure ()

up6 :: Connection -> IO ()
up6 c = do
  migrate ( do
    create_table "file"
      [ column "fileId" "serial PRIMARY KEY"
      , column "fileTitle" "text NOT NULL"
      , column "fileEasyId" "text NOT NULL"
      , column "fileCreated" "timestamptz NOT NULL"
      , column "fileFile" "text NOT NULL"
      ]
    ) c

dropAll :: IO ()
dropAll = do
  c <- connection
  _ <- dropTableCascade "post" c
  _ <- dropTableCascade "comment" c
  _ <- dropTableCascade "\"user\"" c
  pure ()

executeQuery_ :: Query -> Migration Int64
executeQuery_ q = ask >>= \conn -> liftIO $ execute_ conn q

dropTableCascade :: String  -> Connection -> IO Int64
dropTableCascade x c = do
  let z = mconcat ( D.List.intersperse " " ["drop table", x, "cascade"] :: [String])
  print z
  execute_ c (fromString $ z)

data ManyToOne a b = ManyToOne a [b]

-- | Run @action@ every @period@ seconds.
runPeriodicallyBigDrift :: Double -> IO () -> IO ()
runPeriodicallyBigDrift period action = forever $ do
  action
  threadDelay (round $ period * 10 ** 6)

migrationsUp :: IO ()
migrationsUp = do
  connection >>= up

appState :: String
appState = "data/appstate.bin"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> pure ()
    ("--migrationsUp":[]) -> migrationsUp >> exitWith ExitSuccess
    ("--migrationsUp":"2":[]) -> connection >>= up2 >> exitWith ExitSuccess
    ("--migrationsUp":"3":[]) -> connection >>= up3 >> exitWith ExitSuccess
    ("--migrationsUp":"4":[]) -> connection >>= up4 >> exitWith ExitSuccess
    ("--migrationsUp":"5":[]) -> connection >>= up5 >> exitWith ExitSuccess
    ("--migrationsUp":"6":[]) -> connection >>= up6 >> exitWith ExitSuccess
    ("--migrate":[]) -> migrateAll >> exitWith ExitSuccess
    ("--dropAll":[]) -> dropAll >> exitWith ExitSuccess
    ("--init":[]) -> do
      encodeFile appState $ AppState mempty []
      writeFile "data/exception.log" ""
      >> exitWith ExitSuccess
    (_) -> print "Possibly unknown args"

  -- encodeFile appState $ AppState mempty []
  let port = case args of
        ("--port":x:[]) -> read x
        _ -> 3001
  catch ( do
    savedAppState <- decodeFile appState :: IO AppState
    sync <- newTVarIO savedAppState
    _ <- forkIO $ do
      let day = 60 * 600 
      threadDelay (day * 10^6)
      runPeriodicallyBigDrift (fromIntegral day) $ do
        nowTime <- liftIO getCurrentTime
        let expire = addUTCTime (fromIntegral $ day * 2) nowTime
        print "Clearing sessions"
        atomically $ modifyTVar' sync (clearOldSessions expire)
    _ <- forkIO $ do
      let s = 3000
      threadDelay (s * 10^6)
      runPeriodicallyBigDrift (fromIntegral s) $ do
        print "Saving session"
        as <- atomically $ readTVar sync
        encodeFile appState as
    let runActionToIO m = runReaderT (runScottySessionT m) sync
    let logger = logStdoutDev
    scottyT port runActionToIO $ do
      defaultHandler $ (\e -> do
                           liftIO $ print $ show e
                           nowtime <- liftIO getCurrentTime
                           liftIO $ appendFile "data/exception.log" $ show nowtime ++ ":"++ show e ++ "\n"
                           html $ e
                           html "An error has occurred. Sincere apologies dear visitor! This incident has been logged and I'll be notified to resolve this as soon as possible.")
      server logger
    )
    (\e -> do
        print ("An exception occurred..." :: String)
        putStrLn $ show (e :: SomeException)
        nowTime <- liftIO getCurrentTime
        appendFile "data/exception.log" $ show nowTime ++ ":"++ show e
    )
