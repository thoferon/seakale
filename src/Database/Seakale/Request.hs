{-# LANGUAGE OverloadedStrings #-}

module Database.Seakale.Request
  ( query
  , query_
  , queryWith
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Request.Internal (MonadRequest, throwError)
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
queryWith = go Proxy
  where
    go :: (MonadRequest b m, ToRow b n r) => Proxy b -> RowParser b s -> Query n
       -> r -> m [s]
    go proxy parser req dat = do
      (cols, rows) <- I.query $ formatQuery req $ toRow proxy dat
      case parseRows parser cols rows of
        Left err -> throwError $ RowParseError err
        Right xs -> return xs

formatQuery :: Query n -> QueryData n -> BSL.ByteString
formatQuery r d = BSL.fromChunks $ go r d
  where
    go :: Query n -> QueryData n -> [BS.ByteString]
    go req dat = case (req, dat) of
      (Plain bs req', _) -> bs : go req' dat
      (Hole req', Cons bs dat') -> bs : go req' dat'
      (EmptyQuery, Nil) -> []
      _ -> error "formatQuery: the impossible happened"
