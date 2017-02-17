import GHC.Generics

import Control.Monad
import Control.Monad.Reader

import System.IO

import Database.Seakale.PostgreSQL hiding (withConn)
import Database.Seakale.PostgreSQL.FromRow
import Database.Seakale.Store

import Shared

newtype TableStats = TableStats { numRows :: Integer } deriving Generic

instance Storable PSQL Two One TableStats where
  data EntityID TableStats = TableStatsID String String deriving Generic

  relation _ = Relation
    { relationName      = "pg_stat_user_tables"
    , relationIDColumns = ["schemaname", "relname"]
    , relationColumns   = ["n_live_tup"]
    }

instance FromRow PSQL One TableStats
instance FromRow PSQL Two (EntityID TableStats)

dbProg :: Select [Entity TableStats]
dbProg = select mempty $ asc EntityID

main :: IO ()
main = withConn $ \conn -> do
  eRes <- runReaderT (runRequest defaultPSQL (runSelectT dbProg)) conn
  case eRes of
    Left err -> hPutStrLn stderr $ "Error: " ++ show err
    Right res ->
      forM_ res $ \(Entity (TableStatsID schemaName relName) TableStats{..}) ->
        putStrLn $ "Table " ++ schemaName ++ "." ++ relName ++ " has around "
                   ++ show numRows ++ " rows."
