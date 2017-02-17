import GHC.Generics

import Control.Monad.Reader

import System.IO

import Database.Seakale.PostgreSQL hiding (withConn)
import Database.Seakale.PostgreSQL.FromRow
import Database.Seakale.Request

import Shared

data UserTable = UserTable
  { schemaName :: String
  , relName    :: String
  , numRows    :: Integer
  } deriving Generic

instance FromRow PSQL Three UserTable

dbProg :: Request [UserTable]
dbProg = do
  let req =
        Plain "SELECT schemaname, relname, n_live_tup FROM pg_stat_user_tables"
              EmptyQuery
  query_ req

main :: IO ()
main = withConn $ \conn -> do
  eRes <- runReaderT (runRequest defaultPSQL dbProg) conn
  case eRes of
    Left err -> hPutStrLn stderr $ "Error: " ++ show err
    Right res -> forM_ res $ \UserTable{..} ->
      putStrLn $ "Table " ++ schemaName ++ "." ++ relName ++ " has around "
                 ++ show numRows ++ " rows."
