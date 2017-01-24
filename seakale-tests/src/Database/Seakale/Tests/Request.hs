module Database.Seakale.Tests.Request
  ( RequestMock
  , mockMatchingQuery
  , mockMatchingExecute
  , mockQuery
  , mockExecute
  , runRequestT
  , runRequest
  , runRequestT'
  , runRequest'
  , module Database.Seakale.Tests.Mock
  ) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Free
import qualified Control.Monad.Except as E

import           Data.Monoid
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Request.Internal
                   hiding (runRequestT, runRequest)
import           Database.Seakale.Types hiding (runQuery, runExecute)

import           Database.Seakale.Tests.Mock

data RequestMock backend
  = MockQuery QueryPredicate ([ColumnInfo backend], [Row backend])
  | MockExecute QueryPredicate Integer

mockMatchingQuery :: (BSL.ByteString -> Bool)
                  -> ([ColumnInfo backend], [Row backend])
                  -> Mock (RequestMock backend) ()
mockMatchingQuery f dat = Action $ MockQuery (QPredFunction f) dat

mockMatchingExecute :: (BSL.ByteString -> Bool) -> Integer
                    -> Mock (RequestMock backend) ()
mockMatchingExecute f i = Action $ MockExecute (QPredFunction f) i

mockQuery :: BSL.ByteString -> ([ColumnInfo backend], [Row backend])
          -> Mock (RequestMock backend) ()
mockQuery req dat = Action $ MockQuery (QPredPlain req) dat

mockExecute :: BSL.ByteString -> Integer -> Mock (RequestMock backend) ()
mockExecute req i = Action $ MockExecute (QPredPlain req) i

runQuery :: BSL.ByteString -> Mock (RequestMock backend) a
         -> Maybe ( ([ColumnInfo backend], [Row backend])
                  , Mock (RequestMock backend) a )
runQuery req = consumeMock $ \case
  MockQuery p cr | runQueryPredicate p req -> Just cr
  _ -> Nothing

runExecute :: BSL.ByteString -> Mock (RequestMock backend) a
           -> Maybe (Integer, Mock (RequestMock backend) a)
runExecute req = consumeMock $ \case
  MockExecute p i | runQueryPredicate p req -> Just i
  _ -> Nothing

runRequestT :: Monad m => backend -> Mock (RequestMock backend) b
            -> RequestT backend m a -> m (Either SeakaleError a)
runRequestT b m = fmap fst . runRequestT' b m

runRequest :: backend -> Mock (RequestMock backend) b -> Request backend a
           -> Either SeakaleError a
runRequest b m = fst . runRequest' b m

runRequestT' :: Monad m => backend -> Mock (RequestMock backend) b
             -> RequestT backend m a
             -> m (Either SeakaleError a, Mock (RequestMock backend) b)
runRequestT' b m =
    flip runStateT m . E.runExceptT . iterT (interpreter b)
   . hoistFreeT (lift . lift)

  where
    interpreter :: Monad m => backend
                -> RequestF backend
                            (E.ExceptT SeakaleError
                             (StateT (Mock (RequestMock backend) b) m) a)
                -> E.ExceptT SeakaleError
                    (StateT (Mock (RequestMock backend) b) m) a
    interpreter backend = \case
      Query req f -> do
        mock <- get
        case runQuery req mock of
          Nothing -> E.throwError $ BackendError $
            "no mock found for Query on " <> BSL.toStrict req
          Just (res, mock') -> put mock' >> f res

      Execute req f -> do
        mock <- get
        case runExecute req mock of
          Nothing -> E.throwError $ BackendError $
            "no mock found for Execute on " <> BSL.toStrict req
          Just (res, mock') -> put mock' >> f res

      GetBackend f              -> f backend
      ThrowError err            -> E.throwError err
      CatchError action handler -> E.catchError action handler

runRequest' :: backend -> Mock (RequestMock backend) b -> Request backend a
            -> (Either SeakaleError a, Mock (RequestMock backend) b)
runRequest' backend mock = runIdentity . runRequestT' backend mock
