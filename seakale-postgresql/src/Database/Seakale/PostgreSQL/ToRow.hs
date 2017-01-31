{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Seakale.PostgreSQL.ToRow
  ( module Database.Seakale.ToRow
  ) where

import           Data.List
import           Data.Monoid
import           Data.Time
import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.ToRow
import           Database.Seakale.Types

import           Database.Seakale.PostgreSQL

instance ToRow PSQL One Bool where
  toRow _ = \case
    True  -> Cons "'t'" Nil
    False -> Cons "'f'" Nil

instance ToRow PSQL One UTCTime where
  toRow backend = toRow backend . formatTime defaultTimeLocale "%F %T%QZ"

instance ToRow PSQL One String where
  toRow backend = toRow backend . BS.pack

instance {-# OVERLAPPABLE #-} ToRow PSQL One a => ToRow PSQL One [a] where
  toRow backend =
    singleton . ("'{" <>) . (<> "}'") . mconcat . intersperse ","
    . map (("\"" <>) . (<> "\"") . escapeByteString)
    . (>>= vectorToList . toRow backend)

escapeByteString :: BS.ByteString -> BS.ByteString
escapeByteString bs =
  case fmap (BS.splitAt 1) (BS.span (\c -> c /= '\\' && c /= '"') bs) of
    (bs', ("\\", bs'')) -> bs' <> "\\\\" <> escapeByteString bs''
    (bs', ("\"", bs'')) -> bs' <> "\\\"" <> escapeByteString bs''
    (bs', _) -> bs'
