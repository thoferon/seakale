module Database.Seakale.PostgreSQL.Types
  ( SeakaleError(..)
  , T.Query(..)
  , T.Only(..)
  , RequestT
  , Request
  , runRequestT
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Free

import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import           Database.PostgreSQL.LibPQ hiding (status, Row)

import           Database.Seakale.Types
                   ( SeakaleError(..), Backend(MonadBackend), ColumnInfo(..)
                   , Row, Field(..) )
import qualified Database.Seakale.Types as T
import qualified Database.Seakale.Request.Internal as I

data PSQL

type RequestT = I.RequestT PSQL
type Request  = I.Request  PSQL

class Monad m => HasConnection m where
  withConn :: (Connection -> m a) -> m a

instance HasConnection m => HasConnection (ExceptT e m) where
  withConn f = do
    eRes <- lift $ withConn $ runExceptT . f
    either throwError return eRes

instance HasConnection m => HasConnection (StateT s m) where
  withConn f = do
    s <- get
    (x, s') <- lift $ withConn $ flip runStateT s . f
    put s'
    return x

type TypeCache = [(Oid, BS.ByteString)]

instance Backend PSQL where
  type ColumnType PSQL = BS.ByteString

  type MonadBackend PSQL m = ( HasConnection m
                             , MonadState TypeCache m
                             , MonadIO m
                             )

  runQuery   _ = runExceptT . runQuery
  runExecute _ = runExceptT . runExecute

runRequestT :: (HasConnection m, MonadIO m) => RequestT m a
            -> m (Either SeakaleError a)
runRequestT = fmap fst . flip runStateT [] . I.runRequestT . hoistFreeT lift

runRequest :: (HasConnection m, MonadIO m) => Request a
           -> m (Either SeakaleError a)
runRequest = runRequestT . hoistFreeT (return . runIdentity)

runQuery :: MonadBackend PSQL m => BSL.ByteString
         -> ExceptT BS.ByteString m ([ColumnInfo PSQL], [Row PSQL])
runQuery lazyReq = do
  let req = mconcat $ BSL.toChunks lazyReq
  res <- exec' req

  let _until i = takeWhile (/= i) $ iterate (+1) 0
  ncols <- liftIO $ nfields res
  nrows <- liftIO $ ntuples res

  let colIndices = _until ncols
  cols <- forM colIndices $ \col -> do
    name <- liftIO $ fname res col
    oid  <- liftIO $ ftype res col
    typ  <- resolveType oid
    return ColumnInfo
      { colInfoName = name
      , colInfoType = typ
      }

  rows <- liftIO $ forM (_until nrows) $ \row -> do
    forM colIndices $ \col -> do
      mValue <- getvalue' res row col
      return Field { fieldValue = mValue }

  return (cols, rows)

runExecute :: MonadBackend PSQL m => BSL.ByteString
           -> ExceptT BS.ByteString m Integer
runExecute lazyReq = do
  let req = mconcat $ BSL.toChunks lazyReq
  res <- exec' req

  mBS <- liftIO $ cmdTuples res
  case fmap (reads . BS.unpack) mBS of
    Just ((n,"") : _) -> return n
    _ -> throwError $ "Can't get number of rows affected for " <> req

resolveType :: MonadBackend PSQL m => Oid
            -> ExceptT BS.ByteString m BS.ByteString
resolveType oid = do
  cache <- get
  case lookup oid cache of
    Just n  -> return n
    Nothing -> do
      cache' <- getTypes
      put cache'
      case lookup oid cache' of
        Just n  -> return n
        Nothing -> throwError $ BS.pack $ "Can't resolve type for " ++ show oid

getTypes :: MonadBackend PSQL m => ExceptT BS.ByteString m TypeCache
getTypes = do
  res <- exec' "SELECT oid, typname FROM pg_type"
  let _until i = takeWhile (/= i) $ iterate (+1) 0
  nrows <- liftIO $ ntuples res

  forM (_until nrows) $ \row -> do
    oidBS  <- liftIO $ getvalue' res row 0
    nameBS <- liftIO $ getvalue' res row 1
    case (fmap (reads . BS.unpack) oidBS, nameBS) of
      (Just ((i,"") : _), Just name) ->
        return (Oid (fromInteger i), name)
      _ -> throwError "Can't read types from pg_type"

exec' :: MonadBackend PSQL m => BS.ByteString
      -> ExceptT BS.ByteString m Result
exec' req = do
  res <- withConn $ \conn -> do
    mRes <- liftIO $ exec conn req
    case mRes of
      Nothing -> do
        err <- liftIO $ fromMaybe "Fatal error" <$> errorMessage conn
        throwError err
      Just r -> return r

  let err bs = do
        msg <- liftIO $ fromMaybe bs <$> resultErrorMessage res
        throwError msg
  status <- liftIO $ resultStatus res
  case status of
    EmptyQuery  -> err "Empty query"
    CopyIn      -> err "COPY not supported"
    CopyOut     -> err "COPY not supported"
    BadResponse -> err "Bad response"
    FatalError  -> err "Fatal error"
    _ -> return ()

  return res
