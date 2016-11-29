{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Seakale.PostgreSQL.ToRow
  ( module Database.Seakale.PostgreSQL.ToRow
  ) where

import qualified Database.Seakale.ToRow as S

import           Database.Seakale.PostgreSQL.Types

type ToRow = S.ToRow PSQL

instance S.ToRow PSQL One Bool where
  toRow _ = \case
    True  -> Cons "'t'" Nil
    False -> Cons "'f'" Nil
