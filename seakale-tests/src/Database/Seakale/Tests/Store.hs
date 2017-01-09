module Database.Seakale.Tests.Store
  ( StoreMock
  , mockSelect
  , mockCount
  , mockSelectJoin
  , mockCountJoin
  , mockInsert
  , mockUpdate
  , mockDelete
  , runSelect
  , runSelectT
  , runStore
  , runStoreT
  , runSelect'
  , runSelectT'
  , runStore'
  , runStoreT'
  , module Database.Seakale.Tests.Mock
  ) where

import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Free
import qualified Control.Monad.Except as E

import           Data.Monoid
import           Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Database.Seakale.Store.Internal
                   hiding (runSelect, runSelectT, runStore, runStoreT)
import           Database.Seakale.Store.Join
import           Database.Seakale.ToRow
import           Database.Seakale.Types

import           Database.Seakale.Tests.Mock

data StoreMock backend
  = forall k l a. Storable backend k l a
    => MockSelect (backend -> Relation backend k l a) (Condition backend a)
                  (SelectClauses backend a) [Entity a]
  | forall k l a. Storable backend k l a
    => MockCount (backend -> Relation backend k l a) (Condition backend a)
                 Integer
  | forall k l a. (Storable backend k l a, Eq a) => MockInsert [a] [EntityID a]
  | forall k l a. Storable backend k l a
    => MockUpdate (UpdateSetter backend a) (Condition backend a) Integer
  | forall k l a. Storable backend k l a
    => MockDelete (Condition backend a) Integer

mockSelect :: Storable backend k l a => Condition backend a
           -> SelectClauses backend a -> [Entity a]
           -> Mock (StoreMock backend) ()
mockSelect cond clauses ents = Action $ MockSelect relation cond clauses ents

mockCount :: Storable backend k l a => Condition backend a -> Integer
          -> Mock (StoreMock backend) ()
mockCount cond n = Action $ MockCount relation cond n

mockSelectJoin :: Storable backend k l a => JoinRelation backend k l a
               -> Condition backend a -> SelectClauses backend a -> [Entity a]
               -> Mock (StoreMock backend) ()
mockSelectJoin rel cond clauses ents = Action $ MockSelect rel cond clauses ents

mockCountJoin :: Storable backend k l a => JoinRelation backend k l a
              -> Condition backend a -> Integer -> Mock (StoreMock backend) ()
mockCountJoin rel cond n = Action $ MockCount rel cond n

mockInsert :: (Storable backend k l a, Eq a) => [a] -> [EntityID a]
           -> Mock (StoreMock backend) ()
mockInsert vals ids = Action $ MockInsert vals ids

mockUpdate :: Storable backend k l a => UpdateSetter backend a
           -> Condition backend a -> Integer -> Mock (StoreMock backend) ()
mockUpdate setter cond n = Action $ MockUpdate setter cond n

mockDelete :: Storable backend k l a => Condition backend a -> Integer
           -> Mock (StoreMock backend) ()
mockDelete cond n = Action $ MockDelete cond n

fakeSelect :: Storable backend k l a => backend -> Relation backend k l a
           -> Condition backend a -> SelectClauses backend a
           -> Mock (StoreMock backend) b
           -> Maybe ([Entity a], Mock (StoreMock backend) b)
fakeSelect backend rel cond clauses = consumeMock $ \case
  MockSelect frel cond' clauses' ents -> do
    ents'     <- cast ents
    rel'      <- cast $ frel backend
    cond''    <- cast cond'
    clauses'' <- cast clauses'
    guard $ eqRelation rel rel' && eqCondition backend cond cond''
            && eqSelectClauses backend clauses clauses''
    return ents'
  _ -> Nothing

fakeCount :: (Storable backend k l a, Typeable backend) => backend
          -> Relation backend k l a -> Condition backend a
          -> Mock (StoreMock backend) b
          -> Maybe (Integer, Mock (StoreMock backend) b)
fakeCount backend rel cond = consumeMock $ \case
  MockCount frel cond' n -> do
    rel'   <- cast $ frel backend
    cond'' <- cast cond'
    guard $ eqRelation rel rel' && eqCondition backend cond cond''
    return n
  _ -> Nothing

runSelect :: Typeable backend => backend -> Mock (StoreMock backend) b
          -> Select backend a -> Either SeakaleError a
runSelect b m = fst . runSelect' b m

runSelect' :: Typeable backend => backend -> Mock (StoreMock backend) b
           -> Select backend a
           -> (Either SeakaleError a, Mock (StoreMock backend) b)
runSelect' b m = runIdentity . runSelectT' b m

runSelectT :: (Monad m, Typeable backend) => backend
           -> Mock (StoreMock backend) b -> SelectT backend m a
           -> m (Either SeakaleError a)
runSelectT b m = fmap fst . runSelectT' b m

runSelectT' :: (Monad m, Typeable backend) => backend
            -> Mock (StoreMock backend) b -> SelectT backend m a
            -> m (Either SeakaleError a, Mock (StoreMock backend) b)
runSelectT' b m = flip runStateT m . E.runExceptT . runSelectHelper b

runSelectHelper :: (Monad m, Typeable backend) => backend -> SelectT backend m a
                -> E.ExceptT SeakaleError
                    (StateT (Mock (StoreMock backend) b) m) a
runSelectHelper b = iterT (interpreter b) . hoistFreeT (lift . lift)
  where
    interpreter :: (Monad m, Typeable backend) => backend
                -> SelectF backend
                           (E.ExceptT SeakaleError
                            (StateT (Mock (StoreMock backend) b) m) a)
                -> E.ExceptT SeakaleError
                    (StateT (Mock (StoreMock backend) b) m) a
    interpreter backend = \case
      Select rel cond clauses f -> do
        mock <- get
        case fakeSelect backend rel cond clauses mock of
          Nothing -> do
            let req = buildSelectRequest backend rel cond clauses
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (ents, mock') -> put mock' >> f ents

      Count rel cond f -> do
        mock <- get
        case fakeCount backend rel cond mock of
          Nothing -> do
            let req = buildCountRequest backend rel cond
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (n, mock') -> put mock' >> f n

      SelectThrowError err -> E.throwError err
      SelectGetBackend f   -> f backend

fakeInsert :: Storable backend k l a => [a] -> Mock (StoreMock backend) b
           -> Maybe ([EntityID a], Mock (StoreMock backend) b)
fakeInsert vals = consumeMock $ \case
  MockInsert vals' ids -> do
    vals'' <- cast vals
    ids'   <- cast ids
    guard $ vals' == vals''
    return ids'
  _ -> Nothing

fakeUpdate :: (Storable backend k l a, Typeable backend) => backend
           -> UpdateSetter backend a
           -> Condition backend a -> Mock (StoreMock backend) b
           -> Maybe (Integer, Mock (StoreMock backend) b)
fakeUpdate backend setter cond = consumeMock $ \case
  MockUpdate setter' cond' n -> do
    setter'' <- cast setter'
    cond''   <- cast cond'
    guard $ eqUpdateSetter backend setter setter''
            && eqCondition backend cond cond''
    return n
  _ -> Nothing

fakeDelete :: Storable backend k l a => backend -> Condition backend a
           -> Mock (StoreMock backend) b
           -> Maybe (Integer, Mock (StoreMock backend) b)
fakeDelete backend cond = consumeMock $ \case
  MockDelete cond' n -> do
    cond'' <- cast cond'
    guard $ eqCondition backend cond cond''
    return n
  _ -> Nothing

runStore :: Typeable backend => backend -> Mock (StoreMock backend) b
         -> Store backend a -> Either SeakaleError a
runStore b m = fst . runStore' b m

runStore' :: Typeable backend => backend -> Mock (StoreMock backend) b
          -> Store backend a
          -> (Either SeakaleError a, Mock (StoreMock backend) b)
runStore' b m = runIdentity . runStoreT' b m

runStoreT :: (Monad m, Typeable backend) => backend
          -> Mock (StoreMock backend) b -> StoreT backend m a
          -> m (Either SeakaleError a)
runStoreT b m = fmap fst . runStoreT' b m

runStoreT' :: (Monad m, Typeable backend) => backend
           -> Mock (StoreMock backend) b -> StoreT backend m a
           -> m (Either SeakaleError a, Mock (StoreMock backend) b)
runStoreT' b m =
    flip runStateT m . E.runExceptT . iterT (interpreter b)
    . hoistFreeT (runSelectHelper b)

  where
    interpreter :: Monad m => backend
                -> StoreF backend
                          (E.ExceptT SeakaleError
                           (StateT (Mock (StoreMock backend) b) m) a)
                -> E.ExceptT SeakaleError
                    (StateT (Mock (StoreMock backend) b) m) a
    interpreter backend = \case
      Insert dat f -> do
        mock <- get
        case fakeInsert dat mock of
          Nothing -> do
            let req = buildInsertRequest (relationOfXs backend dat)
                                         (map (toRow backend) dat)
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (ids, mock') -> put mock' >> f ids

      Update setter cond f -> do
        mock <- get
        case fakeUpdate backend setter cond mock of
          Nothing -> do
            let req = buildUpdateRequest backend (relation backend) setter cond
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (n, mock') -> put mock' >> f n

      Delete cond f -> do
        mock <- get
        case fakeDelete backend cond mock of
          Nothing -> do
            let req = buildDeleteRequest backend (relation backend) cond
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (n, mock') -> put mock' >> f n

    relationOfXs :: Storable backend k l a => backend -> [a]
                 -> Relation backend k l a
    relationOfXs backend _ = relation backend
