module Shared where

import Control.Exception

import System.IO

import Database.Seakale.PostgreSQL

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

getConnectInfo :: IO ConnectInfo
getConnectInfo = ConnectInfo
  <$> prompt "Hostname: "
  <*> fmap read (prompt "Port: ")
  <*> prompt "Username: "
  <*> prompt "Password: "
  <*> prompt "Database: "

withConn :: (Connection -> IO a) -> IO a
withConn f = do
  ci <- getConnectInfo
  bracket (connect ci) disconnect f
