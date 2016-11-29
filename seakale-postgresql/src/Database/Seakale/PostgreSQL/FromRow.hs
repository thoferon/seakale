{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Seakale.PostgreSQL.FromRow
  ( module Database.Seakale.PostgreSQL.FromRow
  , module Database.Seakale.FromRow
  ) where

import           Database.Seakale.FromRow hiding (RowParser, FromRow)
import qualified Database.Seakale.FromRow as S

import           Database.Seakale.PostgreSQL.Types

type RowParser = S.RowParser PSQL
type FromRow   = S.FromRow PSQL
