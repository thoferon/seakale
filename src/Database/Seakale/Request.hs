{-# LANGUAGE OverloadedStrings #-}

module Database.Seakale.Request
  ( query
  , query_
  , queryWith
  ) where

import           Database.Seakale.Request.Internal
                   (MonadRequest, throwError, getBackend)
import           Database.Seakale.FromRow
import           Database.Seakale.ToRow
import           Database.Seakale.Types
import qualified Database.Seakale.Request.Internal as I

query :: (MonadRequest b m, ToRow b n r, FromRow b s) => Query n -> r -> m [s]
query = queryWith fromRow

query_ :: (MonadRequest b m, FromRow b r) => Query Zero -> m [r]
query_ req = query req ()

queryWith :: (MonadRequest b m, ToRow b n r) => RowParser b s -> Query n -> r
          -> m [s]
queryWith parser req dat = do
  backend <- getBackend
  (cols, rows) <- I.query $ formatQuery req $ toRow backend dat
  case parseRows parser backend cols rows of
    Left err -> throwError $ RowParseError err
    Right xs -> return xs
