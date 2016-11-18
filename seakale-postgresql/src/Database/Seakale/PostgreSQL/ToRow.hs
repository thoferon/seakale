{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Seakale.PostgreSQL.ToRow
  ( module Database.Seakale.PostgreSQL.ToRow
  ) where

import qualified Data.ByteString as BS

import           Database.Seakale.Types
import qualified Database.Seakale.ToRow as S

import           Database.Seakale.PostgreSQL.Types

class ToRow n a | a -> n where
  toRow :: a -> QueryData n

class ToField a where
  toField :: a -> BS.ByteString

instance ToRow n a => S.ToRow PSQL n a where
  toRow PSQL = toRow

instance ToField a => S.ToField PSQL a where
  toField PSQL = toField
