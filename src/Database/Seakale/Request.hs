module Database.Seakale.Request
  ( MonadRequest
  , throwSeakaleError
  , getBackend
  , query
  , query_
  , queryWith
  , execute
  , execute_
  , executeMany
  , executeMany_
  , returning
  , returningWith
  , returning_
  , returningWith_
  ) where

import           Database.Seakale.Request.Internal (MonadRequest)
import           Database.Seakale.FromRow
import           Database.Seakale.ToRow
import           Database.Seakale.Types
import qualified Database.Seakale.Request.Internal as I

query :: (MonadRequest b m, ToRow b n r, FromRow b n' s) => Query n -> r
      -> m [s]
query = queryWith fromRow

query_ :: (MonadRequest b m, FromRow b n r) => Query Zero -> m [r]
query_ req = query req ()

queryWith :: (MonadRequest b m, ToRow b n r) => RowParser b n' s -> Query n -> r
          -> m [s]
queryWith parser req dat = do
  backend <- getBackend
  (cols, rows) <- I.query $ formatQuery req $ toRow backend dat
  case parseRows parser backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

execute :: (MonadRequest b m, ToRow b n r) => Query n -> r -> m Integer
execute req dat = do
  backend <- getBackend
  I.execute $ formatQuery req $ toRow backend dat

execute_ :: MonadRequest b m => Query Zero -> m Integer
execute_ req = I.execute $ formatQuery req Nil

executeMany :: (MonadRequest b m, ToRow b n1 r1, ToRow b n2 r2, ToRow b n3 r3)
            => RepeatQuery n1 n2 n3 -> r1 -> r3 -> [r2] -> m Integer
executeMany req bdat adat dat = do
  backend <- getBackend
  I.execute $ formatMany req
    (toRow backend bdat) (toRow backend adat) (map (toRow backend) dat)

executeMany_ :: (MonadRequest b m, ToRow b n r)
             => RepeatQuery Zero n Zero -> [r] -> m Integer
executeMany_ req dat = executeMany req () () dat

returning :: ( MonadRequest b m, ToRow b n1 r1, ToRow b n2 r2, ToRow b n3 r3
             , FromRow b n s )
          => RepeatQuery n1 n2 n3 -> r1 -> r3 -> [r2] -> m [s]
returning = returningWith fromRow

returningWith :: ( MonadRequest b m, ToRow b n1 r1, ToRow b n2 r2, ToRow b n3 r3
                 , FromRow b n s )
              => RowParser b n s -> RepeatQuery n1 n2 n3 -> r1 -> r3 -> [r2]
              -> m [s]
returningWith parser req bdat adat dat = do
  backend <- getBackend
  (cols, rows) <- I.query $ formatMany req
    (toRow backend bdat) (toRow backend adat) (map (toRow backend) dat)
  case parseRows parser backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

returning_ :: (MonadRequest b m, ToRow b n r, FromRow b n' s)
           => RepeatQuery Zero n Zero -> [r] -> m [s]
returning_ req dat = returningWith fromRow req () () dat

returningWith_ :: (MonadRequest b m, ToRow b n r, FromRow b n' s)
               => RowParser b n' s -> RepeatQuery Zero n Zero -> [r] -> m [s]
returningWith_ parser req dat = returningWith parser req () () dat
