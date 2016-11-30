module Main where

import GHC.Generics

import Control.Monad.Reader

import System.IO

import Database.Seakale.PostgreSQL
import Database.Seakale.PostgreSQL.FromRow
import Database.Seakale.PostgreSQL.Types
import Database.Seakale.Request

data UserTable = UserTable
  { schemaName :: String
  , relname    :: String
  , n_live_tup :: Integer
  } deriving (Generic, Show)

instance FromRow Three UserTable

dbProg :: Request [UserTable]
dbProg = do
  let req =
        Plain "SELECT schemaname, relname, n_live_tup FROM pg_stat_user_tables"
              EmptyQuery
  query_ req

prompt :: String -> IO String
prompt str = do
  putStr str
  hFlush stdout
  getLine

main :: IO ()
main = do
  ci <- ConnectInfo
    <$> prompt "Hostname: "
    <*> fmap read (prompt "Port: ")
    <*> prompt "Username: "
    <*> prompt "Password: "
    <*> prompt "Database: "

  conn <- connect ci
  eRes <- runReaderT (runRequest dbProg) conn
  print eRes
  disconnect conn
