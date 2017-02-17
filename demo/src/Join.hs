import GHC.Generics

import Control.Monad
import Control.Monad.Reader

import System.IO

import Database.Seakale.PostgreSQL hiding (withConn)
import Database.Seakale.PostgreSQL.FromRow
import Database.Seakale.Store
import Database.Seakale.Store.Join

import Shared

newtype TableStats = TableStats { numRows :: Integer } deriving Generic

instance Storable PSQL Two One TableStats where
  data EntityID TableStats = TableStatsID String String deriving Generic

  relation _ = Relation
    { relationName      = "pg_stat_user_tables"
    , relationIDColumns = ["schemaname", "relname"]
    , relationColumns   = ["n_live_tup"]
    }

instance FromRow PSQL Two (EntityID TableStats)
instance FromRow PSQL One TableStats

data TableStatsProperty backend n a where
  TableStatsNumRows :: TableStatsProperty PSQL One Integer

instance Property backend TableStats TableStatsProperty where
  toColumns _ TableStatsNumRows = ["n_live_tup"]

data TableInfo = TableInfo
  { hasRules    :: Bool
  , hasTriggers :: Bool
  } deriving Generic

instance Storable PSQL Two Two TableInfo where
  data EntityID TableInfo = TableInfoID String String deriving Generic

  relation _ = Relation
    { relationName      = "pg_tables"
    , relationIDColumns = ["schemaname", "tablename"]
    , relationColumns   = ["hasrules", "hastriggers"]
    }

instance FromRow PSQL Two (EntityID TableInfo)
instance FromRow PSQL Two TableInfo

dbProg :: Select [Entity (LeftJoin TableStats TableInfo)]
dbProg =
  selectJoin (leftJoin_ (JLeft EntityID ==~ JRight EntityID))
             (JLeft TableStatsNumRows >. 0)
             (asc (JLeft TableStatsNumRows))

main :: IO ()
main = withConn $ \conn -> do
  eRes <- runReaderT (runRequest defaultPSQL (runSelectT dbProg)) conn
  case eRes of
    Left err -> hPutStrLn stderr $ "Error: " ++ show err
    Right res ->
      forM_ res $ \(Entity (LeftJoinID (TableStatsID schemaName relName) _)
                           (LeftJoin TableStats{..} mInfo)) -> do
        putStr $ "Table " ++ schemaName ++ "." ++ relName ++ " has around "
                 ++ show numRows ++ " rows"
        case mInfo of
          Nothing -> putStrLn "."
          Just TableInfo{..} ->
            putStrLn $ " (has rules: " ++ show hasRules ++ ", has triggers: "
                       ++ show hasTriggers ++ ")."
