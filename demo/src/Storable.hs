import GHC.Generics

import Control.Monad
import Control.Monad.Reader

import System.IO

import Database.Seakale.PostgreSQL hiding (withConn)
import Database.Seakale.PostgreSQL.FromRow
import Database.Seakale.Storable

import Shared

newtype TableInfo = TableInfo { numRows :: Integer } deriving Generic

instance FromRow PSQL One TableInfo

instance Storable PSQL Two One TableInfo where
  data EntityID TableInfo = TableID String String deriving Generic

  relation = Relation
    { relationName      = "pg_stat_user_tables"
    , relationIDColumns = ["schemaname", "relname"]
    , relationColumns   = ["n_live_tup"]
    }

instance FromRow PSQL Two (EntityID TableInfo)

dbProg :: Select [Entity TableInfo]
dbProg =
  undefined -- FIXME

main :: IO ()
main = withConn $ \conn -> do
  eRes <- runReaderT (runRequest (runSelectT dbProg)) conn
  case eRes of
    Left err -> hPutStrLn stderr $ "Error: " ++ show err
    Right res ->
      forM_ res $ \(Entity (TableID schemaName relName) TableInfo{..}) ->
        putStrLn $ "Table " ++ schemaName ++ "." ++ relName ++ " has around "
                   ++ show numRows ++ " rows."
