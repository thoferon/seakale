{-# LANGUAGE UndecidableInstances #-}

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
  deriving Functor

type RequestT backend = FreeT (RequestF backend)
type Request  backend = RequestT backend Identity

class Monad m => MonadRequest backend m | m -> backend where
  query      :: BSL.ByteString -> m ([ColumnInfo backend], [Row backend])
  execute    :: BSL.ByteString -> m Integer
  throwError :: SeakaleError  -> m a

instance Monad m => MonadRequest backend (FreeT (RequestF backend) m) where
  query   req = liftF $ Query   req id
  execute req = liftF $ Execute req id
  throwError  = liftF . ThrowError

instance {-# OVERLAPPABLE #-} ( MonadRequest backend m, MonadTrans t
                              , Monad (t m) )
  => MonadRequest backend (t m) where
  query      = lift . query
  execute    = lift . execute
  throwError = lift . throwError

runRequestT :: (Backend backend, MonadBackend backend m, Monad m)
            => RequestT backend m b -> m (Either SeakaleError b)
runRequestT = E.runExceptT . iterTM (interpreter Proxy)
  where
    interpreter :: (Backend backend, MonadBackend backend m, Monad m)
                => Proxy backend
                -> RequestF backend (E.ExceptT SeakaleError m a)
                -> E.ExceptT SeakaleError m a
    interpreter proxy = \case
      Query req f -> do
        eRes <- lift $ runQuery proxy req
        either (E.throwError . BackendError) f eRes
      Execute req f -> do
        eRes <- lift $ runExecute proxy req
        either (E.throwError . BackendError) f eRes
      ThrowError err -> E.throwError err

runRequest :: (Backend backend, MonadBackend backend m, Monad m)
           => Request backend b -> m (Either SeakaleError b)
runRequest = runRequestT . hoistFreeT (return . runIdentity)
