module Database.Seakale.Request.Internal where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Free
import qualified Control.Monad.Except as E

import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Types

data RequestF backend a
  = Query   BSL.ByteString (([ColumnInfo backend], [Row backend]) -> a)
  | Execute BSL.ByteString (Integer -> a)
  | GetBackend (backend -> a)
  | ThrowError SeakaleError
  | CatchError a (SeakaleError -> a)
  deriving Functor

type RequestT backend = FreeT (RequestF backend)
type Request  backend = RequestT backend Identity

class MonadSeakaleBase backend m => MonadRequest backend m where
  query   :: BSL.ByteString -> m ([ColumnInfo backend], [Row backend])
  execute :: BSL.ByteString -> m Integer

instance Monad m => MonadSeakaleBase backend (FreeT (RequestF backend) m) where
  getBackend        = liftF $ GetBackend id
  throwSeakaleError = liftF . ThrowError
  catchSeakaleError action handler =
    FreeT $ return $ Free $ CatchError action handler

instance Monad m => MonadRequest backend (FreeT (RequestF backend) m) where
  query   req = liftF $ Query   req id
  execute req = liftF $ Execute req id

instance {-# OVERLAPPABLE #-} ( MonadRequest backend m, MonadTrans t
                              , MonadSeakaleBase backend (t m) )
  => MonadRequest backend (t m) where
  query   = lift . query
  execute = lift . execute

runRequestT :: (Backend backend, MonadBackend backend m, Monad m)
            => backend -> RequestT backend m a -> m (Either SeakaleError a)
runRequestT b = E.runExceptT . iterTM (interpreter b)
  where
    interpreter :: (Backend backend, MonadBackend backend m, Monad m)
                => backend -> RequestF backend (E.ExceptT SeakaleError m a)
                -> E.ExceptT SeakaleError m a
    interpreter backend = \case
      Query req f -> do
        eRes <- lift $ runQuery backend req
        either (E.throwError . BackendError) f eRes

      Execute req f -> do
        eRes <- lift $ runExecute backend req
        either (E.throwError . BackendError) f eRes

      GetBackend f              -> f backend
      ThrowError err            -> E.throwError err
      CatchError action handler -> E.catchError action handler

runRequest :: (Backend backend, MonadBackend backend m, Monad m)
           => backend -> Request backend a -> m (Either SeakaleError a)
runRequest backend = runRequestT backend . hoistFreeT (return . runIdentity)
