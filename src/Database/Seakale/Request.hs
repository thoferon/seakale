module Database.Seakale.Request
  ( query
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
  , MonadRequest
  , throwSeakaleError
  , getBackend
  ) where

import           Database.Seakale.Request.Internal (MonadRequest)
import           Database.Seakale.FromRow
import           Database.Seakale.ToRow
import           Database.Seakale.Types
import qualified Database.Seakale.Request.Internal as I

-- | Replace holes in the query with the provided values and send it to the
-- database. This is to be used for @SELECT@ queries.
query :: (MonadRequest b m, ToRow b n r, FromRow b n' s) => Query n -> r
      -> m [s]
query = queryWith fromRow

-- | Like 'query' but the query should not have any hole.
query_ :: (MonadRequest b m, FromRow b n r) => Query Zero -> m [r]
query_ req = query req ()

-- | Provide a way to specify a custom parser for 'query'.
queryWith :: (MonadRequest b m, ToRow b n r) => RowParser b n' s -> Query n -> r
          -> m [s]
queryWith parser req dat = do
  backend <- getBackend
  (cols, rows) <- I.query $ formatQuery req $ toRow backend dat
  case parseRows parser backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

-- | Replace holes in the query with the provided values, send it to the
-- database and return the number of rows affected. This is to be used with
-- @DELETE@, @UPDATE@ and @INSERT@ queries (without any @RETURNING@ clause).
execute :: (MonadRequest b m, ToRow b n r) => Query n -> r -> m Integer
execute req dat = do
  backend <- getBackend
  I.execute $ formatQuery req $ toRow backend dat

-- | Like 'execute' but the query should not have any hole.
execute_ :: MonadRequest b m => Query Zero -> m Integer
execute_ req = I.execute $ formatQuery req Nil

-- | Like 'execute' but for a 'RepeatQuery' where a piece of the query is
-- repeated as many times as the number of values of type 'r2'.
executeMany :: (MonadRequest b m, ToRow b n1 r1, ToRow b n2 r2, ToRow b n3 r3)
            => RepeatQuery n1 n2 n3 -> r1 -> r3 -> [r2] -> m Integer
executeMany req bdat adat dat = do
  backend <- getBackend
  I.execute $ formatMany req
    (toRow backend bdat) (toRow backend adat) (map (toRow backend) dat)

-- | Like 'executeMany' but the query should not have any hole before and after
-- the repeating piece.
executeMany_ :: (MonadRequest b m, ToRow b n r)
             => RepeatQuery Zero n Zero -> [r] -> m Integer
executeMany_ req dat = executeMany req () () dat

-- | Replace holes in a 'RepeatQuery' and send it to the database. This is to be
-- used for @INSERT@ queries with a @RETURNING@ clause.
returning :: ( MonadRequest b m, ToRow b n1 r1, ToRow b n2 r2, ToRow b n3 r3
             , FromRow b n s )
          => RepeatQuery n1 n2 n3 -> r1 -> r3 -> [r2] -> m [s]
returning = returningWith fromRow

-- | Provide a way to a custom parser for 'returning'.
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

-- | Like 'returning' but the query should not have any hole before and after
-- the repeating piece.
returning_ :: (MonadRequest b m, ToRow b n r, FromRow b n' s)
           => RepeatQuery Zero n Zero -> [r] -> m [s]
returning_ req dat = returningWith fromRow req () () dat

-- | Like 'returningWith' but the query should not have any hole before and
-- after the repeating piece.
returningWith_ :: (MonadRequest b m, ToRow b n r, FromRow b n' s)
               => RowParser b n' s -> RepeatQuery Zero n Zero -> [r] -> m [s]
returningWith_ parser req dat = returningWith parser req () () dat
