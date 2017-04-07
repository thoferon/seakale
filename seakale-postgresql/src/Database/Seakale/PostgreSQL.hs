module Database.Seakale.PostgreSQL
  ( Connection
  , ConnectInfo(..)
  , connect
  , connectString
  , disconnect
  , Request
  , RequestT
  , runRequest
  , runRequestT
  , Select
  , SelectT
  , runSelect
  , runSelectT
  , Store
  , StoreT
  , runStore
  , runStoreT
  , HasConnection(..)
  , TypeInfo(..)
  , TypeType(..)
  , PSQL(..)
  , defaultPSQL
  , SeakaleError(..)
  , T.Query(..) -- prefixed to export EmptyQuery
  , Field(..)
  , Row
  , ColumnInfo(..)
  , QueryData
  , Vector(..)
  , cons, (<:>)
  , nil, (<:|)
  , Zero
  , One
  , Two
  , Three
  , Four
  , Five
  , Six
  , Seven
  , Eight
  , Nine
  , Ten
  -- * Specific types
  , Composite(..)
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Free

import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import           System.IO

import           Database.PostgreSQL.LibPQ hiding (Row, status)

import           Database.Seakale.Types
                   hiding (runQuery, runExecute, EmptyQuery)
import qualified Database.Seakale.Request.Internal as I
import qualified Database.Seakale.Store.Internal as I
import qualified Database.Seakale.Types as T

data ConnectInfo = ConnectInfo
  { ciHostname :: String
  , ciPort     :: Word16
  , ciUsername :: String
  , ciPassword :: String
  , ciDatabase :: String
  }

toConnectionString :: ConnectInfo -> BS.ByteString
toConnectionString ConnectInfo{..} =
    "host=" <> quote ciHostname
    <> " port=" <> BS.pack (show ciPort)
    <> " user=" <> quote ciUsername
    <> " password=" <> quote ciPassword
    <> " dbname=" <> quote ciDatabase

  where
    quote :: String -> BS.ByteString
    quote = ("'" <>) . (<> "'") . escapeQuotes "" . BS.pack

    escapeQuotes :: BS.ByteString -> BS.ByteString -> BS.ByteString
    escapeQuotes _ "" = ""
    escapeQuotes prefix s =
      let (start, end) = fmap (BS.drop 1) $ BS.break (=='\'') s
      in prefix <> start <> escapeQuotes "''" end

connect :: ConnectInfo -> IO Connection
connect = connectString . toConnectionString

connectString :: BS.ByteString -> IO Connection
connectString = connectdb

disconnect :: Connection -> IO ()
disconnect = finish

class Monad m => HasConnection m where
  withConn :: (Connection -> m a) -> m a

instance Monad m => HasConnection (ReaderT Connection m) where
  withConn f = f =<< ask

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

type TypeInfoOid = (BS.ByteString, Maybe [(BS.ByteString, Oid)], Maybe Oid)

data TypeType
  = TTComposite [(BS.ByteString, TypeInfo)]
  | TTArray TypeInfo
  | TTOther
  deriving Show

data TypeInfo = TypeInfo
  { typeName :: BS.ByteString
  , typeType :: TypeType
  } deriving Show

type TypeCache = [(Oid, TypeInfoOid)]

data PSQL = PSQL { psqlLogQueries :: Bool }

defaultPSQL :: PSQL
defaultPSQL = PSQL False

instance Backend PSQL where
  type ColumnType PSQL = TypeInfo

  type MonadBackend PSQL m = ( HasConnection m
                             , MonadState TypeCache m
                             , MonadIO m
                             )

  runQuery   backend = runExceptT . runQuery   backend
  runExecute backend = runExceptT . runExecute backend

type RequestT = I.RequestT PSQL
type Request  = I.Request  PSQL

runRequestT :: (HasConnection m, MonadIO m) => PSQL -> RequestT m a
            -> m (Either SeakaleError a)
runRequestT backend =
  fmap fst . flip runStateT [] . I.runRequestT backend . hoistFreeT lift

runRequest :: (HasConnection m, MonadIO m) => PSQL -> Request a
           -> m (Either SeakaleError a)
runRequest backend = runRequestT backend . hoistFreeT (return . runIdentity)

type SelectT = I.SelectT PSQL
type Select  = I.Select  PSQL

runSelectT :: Monad m => SelectT m a -> RequestT m a
runSelectT = I.runSelectT

runSelect :: Select a -> Request a
runSelect = I.runSelect

type StoreT m = I.StoreT PSQL m
type Store    = I.Store  PSQL

runStoreT :: Monad m => StoreT m a -> RequestT m a
runStoreT = I.runStoreT

runStore :: Store a -> Request a
runStore = I.runStore

runQuery :: MonadBackend PSQL m => PSQL -> BSL.ByteString
         -> ExceptT BS.ByteString m ([ColumnInfo PSQL], [Row PSQL])
runQuery PSQL{..} lazyReq = do
  let req = BSL.toStrict lazyReq
  when psqlLogQueries $ liftIO $ BS.hPutStrLn stderr $ "runQuery: " <> req
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

runExecute :: MonadBackend PSQL m => PSQL -> BSL.ByteString
           -> ExceptT BS.ByteString m Integer
runExecute PSQL{..} lazyReq = do
  let req = BSL.toStrict lazyReq
  when psqlLogQueries $ liftIO $ BS.hPutStrLn stderr $ "runExecute: " <> req
  res <- exec' req

  mBS <- liftIO $ cmdTuples res
  case fmap (reads . BS.unpack) mBS of
    Just ((n,"") : _) -> return n
    _ -> throwError $ "Can't get number of rows affected for " <> req

resolveType :: MonadBackend PSQL m => Oid -> ExceptT BS.ByteString m TypeInfo
resolveType oid = do
  (name, mAttrs, mElemOID) <- resolveTypeOid oid
  case (mAttrs, mElemOID) of
    (Nothing, Nothing) -> return $ TypeInfo name TTOther
    (Just attrs, Nothing) -> do
      attrs' <- mapM (\(aname, aoid) -> (aname,) <$> resolveType aoid) attrs
      return $ TypeInfo name $ TTComposite attrs'
    (Nothing, Just eoid) -> do
      etinfo <- resolveType eoid
      return $ TypeInfo name $ TTArray etinfo
    _ -> throwError $ "Can't resolve type " <> name

resolveTypeOid :: MonadBackend PSQL m => Oid
               -> ExceptT BS.ByteString m TypeInfoOid
resolveTypeOid oid = do
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
    -- It uses { for separator as a hack to force the presence of double quotes
    res <- exec' "SELECT pg_type.oid, typname, typtype, typelem, typlen,\
                 \ array_agg(attnum || '{' || attname || '{' || atttypid)\
                 \ FROM pg_type LEFT JOIN pg_attribute ON attrelid = typrelid\
                 \ AND typrelid <> 0 GROUP BY oid, typname, typtype, typelem,\
                 \ typlen"
    let _until i = takeWhile (/= i) $ iterate (+1) 0
    nrows <- liftIO $ ntuples res

    forM (_until nrows) $ \row -> do
      oidBS  <- liftIO $ getvalue' res row 0
      nameBS <- liftIO $ getvalue' res row 1
      typeBS <- liftIO $ getvalue' res row 2
      elemBS <- liftIO $ getvalue' res row 3
      lenBS  <- liftIO $ getvalue' res row 4
      attBS  <- liftIO $ getvalue' res row 5

      case ( fmap (reads . BS.unpack) oidBS, nameBS, typeBS
           , fmap (reads . BS.unpack) elemBS, lenBS, attBS ) of
        (Just ((i,"") : _), Just name, _, Just ((e,""):_), Just "-1", _)
          | e /= 0 -> return ( Oid (fromInteger i)
                             , (name, Nothing, Just (Oid (fromInteger e))) )
        (Just ((i,"") : _), Just name, Just "c", _, _, Just bs) -> do
          attrs <- case readAttrs bs of
            Nothing ->
              throwError $ "Can't read attributes of composite type: " <> bs
            Just as -> return as
          return (Oid (fromInteger i), (name, Just attrs, Nothing))
        (Just ((i,"") : _), Just name, _, _, _, _) ->
          return (Oid (fromInteger i), (name, Nothing, Nothing))
        _ -> throwError "Can't read types from pg_type"

  where
    readAttrs :: BS.ByteString -> Maybe [(BS.ByteString, Oid)]
    readAttrs bs = do
      stripped <- (BS.stripPrefix "{" >=> BS.stripSuffix "}") bs
      bsTuples <- forM (BS.split ',' stripped) $ \sub -> do
        strippedSub <- (BS.stripPrefix "\"" >=> BS.stripSuffix "\"") sub
        return $ BS.split '{' strippedSub

      tuples <- forM bsTuples $ \att -> case att of
        [attnum, attname, atttypid] ->
          case (reads (BS.unpack attnum), reads (BS.unpack atttypid)) of
            ((n,""):_, (i,""):_) ->
              Just (n :: Int, attname, Oid (fromInteger i))
            _ -> Nothing
        _ -> Nothing

      return $ map (\(_,n,i) -> (n,i)) $ sortBy (compare `on` (\(n,_,_) -> n)) $
        filter (\(n,_,_) -> n > 0) tuples

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

newtype Composite a = Composite { fromComposite :: a } deriving (Show, Eq)
