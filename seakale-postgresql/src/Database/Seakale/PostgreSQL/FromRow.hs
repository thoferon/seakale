{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Seakale.PostgreSQL.FromRow
  ( module Database.Seakale.PostgreSQL.FromRow
  , module Database.Seakale.FromRow
  ) where

import           Database.Seakale.FromRow
                   hiding (FromField(..), RowParser(..), FieldParser)
import           Database.Seakale.Types
import qualified Database.Seakale.FromRow as S

import           Database.Seakale.PostgreSQL.Types

type RowParser = S.RowParser PSQL

type FieldParser a = ColumnInfo PSQL -> Field PSQL -> Either String a

class FromField a where
  fromField :: FieldParser a

instance FromField a => S.FromField PSQL a where
  fromField PSQL col f = fromField col f
