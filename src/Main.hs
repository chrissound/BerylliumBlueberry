{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Database.PostgreSQL.Simple
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
import qualified Data.Aeson

-- Local package
import Server
import App
import Database
import AppConfig

-- | Run @action@ every @period@ seconds.
runPeriodicallyBigDrift :: Double -> IO () -> IO ()
runPeriodicallyBigDrift period action = forever $ do
  action
  threadDelay (round $ period * 10 ** 6)

appState :: String
appState = "data/appstate.bin"

initConfig :: IO ()
initConfig = do
      encodeFile appState $ AppState mempty []
      writeFile "data/exception.log" ""
      Data.Aeson.encodeFile "data/config.json" $
        AppConfig "Sample config" "BerylliumBlueberry" ([]) "hello@your-email" "<a href='https://github.com/chrissound/BerylliumBlueberry'>Built with BerylliumBlueberry</a>" ""
          (ConnectInfo "localhost" 5432 "" "" "")
      >> exitWith ExitSuccess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> pure ()
    ("--init":[]) -> initConfig
    (_) -> print "Possibly unknown args"
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
        atomically $ modifyTVar' sync (clearOldSessions expire)
    _ <- forkIO $ do
      let s = 30
      threadDelay (s * 10^6)
      runPeriodicallyBigDrift (fromIntegral s) $ do
        print "saving state..."
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
      server sync logger
    )
    (\e -> do
        print ("An exception occurred..." :: String)
        putStrLn $ show (e :: SomeException)
        nowTime <- liftIO getCurrentTime
        appendFile "data/exception.log" $ show nowTime ++ ":"++ show e
    )
