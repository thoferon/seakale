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
  | ThrowError SeakaleError
  | GetBackend (backend -> a)
  deriving Functor

type RequestT backend = FreeT (RequestF backend)
type Request  backend = RequestT backend Identity

class MonadSeakaleBase backend m => MonadRequest backend m where
  query   :: BSL.ByteString -> m ([ColumnInfo backend], [Row backend])
  execute :: BSL.ByteString -> m Integer

instance Monad m => MonadSeakaleBase backend (FreeT (RequestF backend) m) where
  throwSeakaleError = liftF . ThrowError
  getBackend        = liftF $ GetBackend id

instance Monad m => MonadRequest backend (FreeT (RequestF backend) m) where
  query   req = liftF $ Query   req id
  execute req = liftF $ Execute req id

instance {-# OVERLAPPABLE #-} ( MonadRequest backend m, MonadTrans t
                              , Monad (t m) )
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
      ThrowError err -> E.throwError err
      GetBackend f   -> f backend

runRequest :: (Backend backend, MonadBackend backend m, Monad m)
           => backend -> Request backend a -> m (Either SeakaleError a)
runRequest backend = runRequestT backend . hoistFreeT (return . runIdentity)
