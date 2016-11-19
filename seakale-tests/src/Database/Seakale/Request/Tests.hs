module Database.Seakale.Request.Tests
  ( runRequestT
  , runRequest
  , module Database.Seakale.Request.Tests.Mock
  ) where

import           Control.Applicative
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Free
import qualified Control.Monad.Except as E

import           Data.Functor.Foldable
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Request.Internal
                   hiding (runRequestT, runRequest)
import           Database.Seakale.Types hiding (runQuery, runExecute)

import           Database.Seakale.Request.Tests.Mock

phi :: (forall b. MockF backend b -> Maybe (a, Mock backend c))
    -> MockF backend (Mock backend c, Maybe (a, Mock backend c))
    -> Maybe (a, Mock backend c)
phi f = \case
    mock@(FMockQuery   _ _) -> f mock
    mock@(FMockExecute _ _) -> f mock
    FOr (_, mRes1) (_, mRes2) -> mRes1 <|> mRes2
    FAnd (m1, mRes1) (m2, mRes2) ->
      case (mRes1, mRes2) of
        (Just (x, m1'), _) -> Just (x, noNone And m1' m2)
        (_, Just (x, m2')) -> Just (x, noNone And m1 m2')
        _ -> Nothing
    FAfter (_, mRes1) (m2, _) ->
      fmap (\(x, m1') -> (x, noNone After m1' m2)) mRes1
    FNone -> Nothing
  where
    noNone :: (Mock backend a -> Mock backend a -> Mock backend a)
           -> Mock backend a -> Mock backend a -> Mock backend a
    noNone _ (None _) m = m
    noNone g m1 m2 = g m1 m2

runQuery :: BSL.ByteString -> Mock backend a
         -> Maybe (([ColumnInfo backend], [Row backend]), Mock backend a)
runQuery req = para (phi f)
  where
    f :: MockF backend a
      -> Maybe (([ColumnInfo backend], [Row backend]), Mock backend b)
    f (FMockQuery p cr) | p req = Just (cr, None Nothing)
    f _ = Nothing

runExecute :: BSL.ByteString -> Mock backend a
           -> Maybe (Integer, Mock backend a)
runExecute req = para (phi f)
  where
    f :: MockF backend a -> Maybe (Integer, Mock backend b)
    f (FMockExecute p i) | p req = Just (i, None Nothing)
    f _ = Nothing

runRequestT :: Monad m => backend -> Mock backend b
            -> RequestT backend m a -> m (Either SeakaleError a)
runRequestT b m =
    fmap fst . flip runStateT m . E.runExceptT . iterT (interpreter b)
   . hoistFreeT (lift . lift)
  where
    interpreter :: Monad m => backend
                -> RequestF backend (E.ExceptT SeakaleError
                                               (StateT (Mock backend b) m) a)
                -> E.ExceptT SeakaleError (StateT (Mock backend b) m) a
    interpreter backend = \case
      Query req f -> do
        mock <- get
        case runQuery req mock of
          Nothing -> error $ "no mock found for Query " ++ show req
          Just (res, mock') -> put mock' >> f res

      Execute req f -> do
        mock <- get
        case runExecute req mock of
          Nothing -> error $ "no mock found for Execute " ++ show req
          Just (res, mock') -> put mock' >> f res

      ThrowError err -> E.throwError err
      GetBackend f -> f backend

runRequest :: backend -> Mock backend b -> Request backend a
           -> Either SeakaleError a
runRequest backend mock = runIdentity . runRequestT backend mock
