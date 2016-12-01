{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Seakale.PostgreSQL.ToRow
  ( module Database.Seakale.PostgreSQL.ToRow
  ) where

import Database.Seakale.ToRow

import Database.Seakale.PostgreSQL

instance ToRow PSQL One Bool where
  toRow _ = \case
    True  -> Cons "'t'" Nil
    False -> Cons "'f'" Nil
