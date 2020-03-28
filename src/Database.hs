module Database where

import Database.PostgreSQL.Simple
import AppConfig

connection :: IO Connection
connection = do
  ac <- getAppConfig
  connect $ databaseConnection ac -- defaultConnectInfo { connectHost = "localhost", connectUser="postgres", connectPassword="mysecretpassword" }
