{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Seakale.PostgreSQL.FromRow
  ( module Database.Seakale.FromRow
  ) where

import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.FromRow
import           Database.Seakale.PostgreSQL
import           Database.Seakale.Types

instance FromRow PSQL One Bool where
  fromRow = pconsume `pbind` \(_, f) -> case fieldValue f of
    Nothing  -> pfail "unexpected NULL"
    Just "t" -> preturn True
    Just "f" -> preturn False
    Just bs  -> pfail $ "unreadable boolean: " ++ BS.unpack bs
