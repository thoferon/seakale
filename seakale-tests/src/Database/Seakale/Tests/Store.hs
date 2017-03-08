module Database.Seakale.Tests.Store
  ( StoreMock
  -- * Mocking the database
  , mockSelect
  , mockSelect_
  , mockFailingSelect
  , mockCount
  , mockFailingCount
  , mockGetMany
  , mockGet
  , mockFailingGetMany
  , mockFailingGet
  , mockSelectJoin
  , mockFailingSelectJoin
  , mockCountJoin
  , mockFailingCountJoin
  , mockInsertMany
  , mockInsert
  , mockFailingInsertMany
  , mockFailingInsert
  , mockUpdateMany
  , mockUpdate
  , mockFailingUpdateMany
  , mockFailingUpdate
  , mockDeleteMany
  , mockDelete
  , mockFailingDeleteMany
  , mockFailingDelete
  -- * Running in a fake environment
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

import           Database.Seakale.Store hiding (get)
import           Database.Seakale.Store.Internal
                   hiding (runSelect, runSelectT, runStore, runStoreT)
import           Database.Seakale.Store.Join
import           Database.Seakale.ToRow
import           Database.Seakale.Types

import           Database.Seakale.Tests.Mock

data StoreMock backend
  = forall k l a. Storable backend k l a
    => MockSelect (backend -> Relation backend k l a) (Condition backend a)
                  (SelectClauses backend a) (Either SeakaleError [Entity a])
  | forall k l a. Storable backend k l a
    => MockCount (backend -> Relation backend k l a) (Condition backend a)
                 (Either SeakaleError Integer)
  | forall k l a. (Storable backend k l a, Eq a)
    => MockInsert [a] (Either SeakaleError [EntityID a])
  | forall k l a. Storable backend k l a
    => MockUpdate (UpdateSetter backend a) (Condition backend a)
                  (Either SeakaleError Integer)
  | forall k l a. Storable backend k l a
    => MockDelete (Condition backend a) (Either SeakaleError Integer)

instance Show (StoreMock backend) where
  show = \case
    MockSelect _ _ _ _ -> "MockSelect"
    MockCount  _ _ _   -> "MockSelect"
    MockInsert _ _     -> "MockInsert"
    MockUpdate _ _ _   -> "MockUpdate"
    MockDelete _ _     -> "MockDelete"

mockSelect :: Storable backend k l a => Condition backend a
           -> SelectClauses backend a -> [Entity a]
           -> Mock (StoreMock backend) ()
mockSelect cond clauses ents =
  Action $ MockSelect relation cond clauses (Right ents)

mockSelect_ :: Storable backend k l a => Condition backend a -> [Entity a]
            -> Mock (StoreMock backend) ()
mockSelect_ cond ents = mockSelect cond mempty ents

mockFailingSelect :: Storable backend k l a => Condition backend a
                  -> SelectClauses backend a -> SeakaleError
                  -> Mock (StoreMock backend) ()
mockFailingSelect cond clauses err =
  Action $ MockSelect relation cond clauses (Left err)

mockCount :: Storable backend k l a => Condition backend a -> Integer
          -> Mock (StoreMock backend) ()
mockCount cond n = Action $ MockCount relation cond (Right n)

mockFailingCount :: Storable backend k l a => Condition backend a
                 -> SeakaleError -> Mock (StoreMock backend) ()
mockFailingCount cond err = Action $ MockCount relation cond (Left err)

mockGetMany :: (Storable backend k l a, ToRow backend k (EntityID a))
            => [EntityID a] -> [Entity a] -> Mock (StoreMock backend) ()
mockGetMany ids ents = mockSelect_ (EntityID `inList` ids) ents

mockGet :: (Storable backend k l a, ToRow backend k (EntityID a))
        => EntityID a -> a -> Mock (StoreMock backend) ()
mockGet i v = mockSelect (EntityID ==. i) (limit 1) [Entity i v]

mockFailingGetMany :: (Storable backend k l a, ToRow backend k (EntityID a))
                   => [EntityID a] -> SeakaleError
                   -> Mock (StoreMock backend) ()
mockFailingGetMany ids = mockFailingSelect (EntityID `inList` ids) mempty

mockFailingGet :: (Storable backend k l a, ToRow backend k (EntityID a))
               => EntityID a -> SeakaleError -> Mock (StoreMock backend) ()
mockFailingGet i = mockFailingSelect (EntityID ==. i) (limit 1)

mockSelectJoin :: Storable backend k l a => JoinRelation backend k l a
               -> Condition backend a -> SelectClauses backend a -> [Entity a]
               -> Mock (StoreMock backend) ()
mockSelectJoin rel cond clauses ents =
  Action $ MockSelect rel cond clauses (Right ents)

mockFailingSelectJoin :: Storable backend k l a => JoinRelation backend k l a
                      -> Condition backend a -> SelectClauses backend a
                      -> SeakaleError -> Mock (StoreMock backend) ()
mockFailingSelectJoin rel cond clauses err =
  Action $ MockSelect rel cond clauses (Left err)

mockCountJoin :: Storable backend k l a => JoinRelation backend k l a
              -> Condition backend a -> Integer -> Mock (StoreMock backend) ()
mockCountJoin rel cond n = Action $ MockCount rel cond (Right n)

mockFailingCountJoin :: Storable backend k l a => JoinRelation backend k l a
                     -> Condition backend a -> SeakaleError
                     -> Mock (StoreMock backend) ()
mockFailingCountJoin rel cond err = Action $ MockCount rel cond (Left err)

mockInsertMany :: (Storable backend k l a, Eq a) => [a] -> [EntityID a]
               -> Mock (StoreMock backend) ()
mockInsertMany vals ids = Action $ MockInsert vals (Right ids)

mockInsert :: (Storable backend k l a, Eq a) => a -> EntityID a
           -> Mock (StoreMock backend) ()
mockInsert v i = mockInsertMany [v] [i]

mockFailingInsertMany :: (Storable backend k l a, Eq a) => [a] -> SeakaleError
                      -> Mock (StoreMock backend) ()
mockFailingInsertMany vals err = Action $ MockInsert vals (Left err)

mockFailingInsert :: (Storable backend k l a, Eq a) => a -> SeakaleError
                  -> Mock (StoreMock backend) ()
mockFailingInsert val = mockFailingInsertMany [val]

mockUpdateMany :: Storable backend k l a => UpdateSetter backend a
               -> Condition backend a -> Integer
               -> Mock (StoreMock backend) ()
mockUpdateMany setter cond n = Action $ MockUpdate setter cond (Right n)

mockUpdate :: (Storable backend k l a, ToRow backend k (EntityID a))
           => EntityID a -> UpdateSetter backend a
           -> Mock (StoreMock backend) ()
mockUpdate i setter = mockUpdateMany setter (EntityID ==. i) 1

mockFailingUpdateMany :: Storable backend k l a => UpdateSetter backend a
                      -> Condition backend a -> SeakaleError
                      -> Mock (StoreMock backend) ()
mockFailingUpdateMany setter cond err =
  Action $ MockUpdate setter cond (Left err)

mockFailingUpdate :: (Storable backend k l a, ToRow backend k (EntityID a))
                  => EntityID a -> UpdateSetter backend a -> SeakaleError
                  -> Mock (StoreMock backend) ()
mockFailingUpdate i setter = mockFailingUpdateMany setter (EntityID ==. i)

mockDeleteMany :: Storable backend k l a => Condition backend a -> Integer
               -> Mock (StoreMock backend) ()
mockDeleteMany cond n = Action $ MockDelete cond (Right n)

mockDelete :: (Storable backend k l a, ToRow backend k (EntityID a))
           => EntityID a -> Mock (StoreMock backend) ()
mockDelete i = mockDeleteMany (EntityID ==. i) 1

mockFailingDeleteMany :: Storable backend k l a => Condition backend a
                      -> SeakaleError -> Mock (StoreMock backend) ()
mockFailingDeleteMany cond err = Action $ MockDelete cond (Left err)

mockFailingDelete :: (Storable backend k l a, ToRow backend k (EntityID a))
                  => EntityID a -> SeakaleError -> Mock (StoreMock backend) ()
mockFailingDelete i = mockFailingDeleteMany (EntityID ==. i)

fakeSelect :: Storable backend k l a => backend -> Relation backend k l a
           -> Condition backend a -> SelectClauses backend a
           -> Mock (StoreMock backend) b
           -> Maybe (Either SeakaleError [Entity a], Mock (StoreMock backend) b)
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
          -> Maybe (Either SeakaleError Integer, Mock (StoreMock backend) b)
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
          Just (ents, mock') -> do
            put mock'
            either E.throwError f ents

      Count rel cond f -> do
        mock <- get
        case fakeCount backend rel cond mock of
          Nothing -> do
            let req = buildCountRequest backend rel cond
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (n, mock') -> do
            put mock'
            either E.throwError f n

      SelectGetBackend f              -> f backend
      SelectThrowError err            -> E.throwError err
      SelectCatchError action handler -> E.catchError action handler

fakeInsert :: Storable backend k l a => [a] -> Mock (StoreMock backend) b
           -> Maybe ( Either SeakaleError [EntityID a]
                    , Mock (StoreMock backend) b )
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
           -> Maybe (Either SeakaleError Integer, Mock (StoreMock backend) b)
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
           -> Maybe (Either SeakaleError Integer, Mock (StoreMock backend) b)
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
          Just (ids, mock') -> do
            put mock'
            either E.throwError f ids

      Update setter cond f -> do
        mock <- get
        case fakeUpdate backend setter cond mock of
          Nothing -> do
            let req = buildUpdateRequest backend (relation backend) setter cond
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (n, mock') -> do
            put mock'
            either E.throwError f n

      Delete cond f -> do
        mock <- get
        case fakeDelete backend cond mock of
          Nothing -> do
            let req = buildDeleteRequest backend (relation backend) cond
            E.throwError $ BackendError $
              "no mock found for request: " <> BSL.toStrict req
          Just (n, mock') -> do
            put mock'
            either E.throwError f n

    relationOfXs :: Storable backend k l a => backend -> [a]
                 -> Relation backend k l a
    relationOfXs backend _ = relation backend
