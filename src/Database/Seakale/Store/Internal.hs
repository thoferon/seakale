module Database.Seakale.Store.Internal where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Free

import           Data.List hiding (insert, delete)
import           Data.Monoid
import           Data.String
import           Data.Typeable
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Database.Seakale.FromRow
import           Database.Seakale.ToRow
import           Database.Seakale.Types
import           Database.Seakale.Request.Internal

-- | A value together with its identifier.
data Entity a = Entity
  { entityID  :: EntityID a
  , entityVal :: a
  }

deriving instance (Show (EntityID a), Show a) => Show (Entity a)
deriving instance (Eq   (EntityID a), Eq   a) => Eq   (Entity a)

instance (FromRow backend k (EntityID a), FromRow backend l a, (k :+ l) ~ i)
  => FromRow backend i (Entity a) where
  fromRow = Entity `pmap` fromRow `papply` fromRow

instance (ToRow backend k (EntityID a), ToRow backend l a, (k :+ l) ~ i)
  => ToRow backend i (Entity a) where
  toRow backend (Entity i v) = toRow backend i `vappend` toRow backend v

data Relation backend k l a = Relation
  { relationName      :: RelationName
  , relationIDColumns :: Vector k Column
  , relationColumns   :: Vector l Column
  }

eqRelation :: Relation backend k l a -> Relation backend k l a -> Bool
eqRelation rel rel' =
  relationName rel == relationName rel'
  && relationIDColumns rel `eqVector` relationIDColumns rel'
  && relationColumns rel `eqVector` relationColumns rel'

newtype RelationName
  = RelationName { unRelationName :: BS.ByteString -> BS.ByteString }

instance IsString RelationName where
  fromString str = RelationName $ \prefix ->
    if BS.null prefix
      then fromString str
      else fromString str <> " AS " <> prefix

instance Eq RelationName where
  (==) rel1 rel2 = unRelationName rel1 "" == unRelationName rel2 ""

newtype Column = Column { unColumn :: BS.ByteString -> BS.ByteString }

instance IsString Column where
  fromString str = Column $ \prefix ->
    if BS.null prefix then fromString str else prefix <> "." <> fromString str

instance Eq Column where
  (==) col1 col2 = unColumn col1 "" == unColumn col2 ""

class (Typeable backend, Typeable k, Typeable l, Typeable a)
  => Storable backend (k :: Nat) (l :: Nat) a | a -> k, a -> l where
  data EntityID a :: *
  relation :: backend -> Relation backend k l a

data Condition backend a
  = forall n. Condition (BS.ByteString -> backend -> (Query n, QueryData n))

instance Monoid (Condition backend a) where
  mempty = Condition $ \_ _ -> (EmptyQuery, Nil)
  mappend = combineConditions "AND"

eqCondition :: backend -> Condition backend a -> Condition backend a -> Bool
eqCondition backend (Condition f) (Condition g) =
  let (condQ,  condD)  = f "" backend
      (condQ', condD') = g "" backend
  in condQ `eqQuery` condQ' && condD `eqVector` condD'

combineConditions :: BS.ByteString -> Condition backend a -> Condition backend a
                  -> Condition backend a
combineConditions op (Condition f) (Condition g) =
  Condition $ \prefix backend ->
    let (q1, qdat1) = f prefix backend
        (q2, qdat2) = g prefix backend
        q = case (q1, q2) of
              (EmptyQuery, _) -> q2
              (_, EmptyQuery) -> q1 `qappend` q2 -- just q1 would not compile
              _ -> parenthesiseQuery q1
                   `qappend` Plain (" " <> op <> " ") (parenthesiseQuery q2)
    in (q, qdat1 `vappend` qdat2)

buildCondition :: BS.ByteString -> (backend -> Vector n Column)
               -> (backend -> QueryData n) -> Condition backend a
buildCondition op vec dat =
    Condition $ \prefix backend ->
      (go id (fmap (($ prefix) . unColumn) (vec backend)), dat backend)
  where
    go :: (forall m. Query m -> Query m) -> Vector n BS.ByteString -> Query n
    go _ Nil = EmptyQuery
    go f (Cons x xs) =
      f $ Plain (x <> " " <> op <> " ") $ Hole $ go (Plain " AND ") xs

buildCondition' :: BS.ByteString
                -> (backend -> Vector n Column) -> (backend -> Vector n Column)
                -> Condition backend a
buildCondition' op vec1 vec2 =
  Condition $ \prefix backend ->
    let q = mconcat . intersperse " AND "
          . map (\(col1, col2) -> unColumn col1 prefix
                                  <> " " <> op <> " "
                                  <> unColumn col2 prefix)
          $ zip (vectorToList (vec1 backend)) (vectorToList (vec2 backend))
    in (Plain q EmptyQuery, Nil)

data SelectClauses backend a = SelectClauses
  { selectGroupBy :: backend -> [Column]
  , selectOrderBy :: backend -> (Bool, [Column])
  , selectLimit   :: Maybe Int
  , selectOffset  :: Maybe Int
  }

instance Monoid (SelectClauses backend a) where
  mempty = SelectClauses
    { selectGroupBy = const []
    , selectOrderBy = const (False, [])
    , selectLimit   = Nothing
    , selectOffset  = Nothing
    }
  mappend sc1 sc2 = SelectClauses
    { selectGroupBy = \backend ->
        selectGroupBy sc1 backend ++ selectGroupBy sc2 backend
    , selectOrderBy = \backend ->
        let (desc1, cols1) = selectOrderBy sc1 backend
            (desc2, cols2) = selectOrderBy sc2 backend
        in (desc1 || desc2, cols1 ++ cols2)
    , selectLimit  = maybe (selectLimit sc1) Just (selectLimit sc2)
    , selectOffset = maybe (selectOffset sc1) Just (selectOffset sc2)
    }

eqSelectClauses :: backend -> SelectClauses backend a -> SelectClauses backend a
                -> Bool
eqSelectClauses backend clauses clauses' =
  selectGroupBy clauses backend == selectGroupBy clauses' backend
  && selectOrderBy clauses backend == selectOrderBy clauses' backend
  && selectLimit clauses == selectLimit clauses'
  && selectOffset clauses == selectOffset clauses'

buildWhereClause :: Condition backend a -> BS.ByteString -> backend
                 -> BSL.ByteString
buildWhereClause (Condition f) prefix backend =
  let cond_ = uncurry formatQuery $ f prefix backend
  in if BSL.null cond_ then "" else " WHERE " <> cond_

buildOnClause :: Condition backend a -> BS.ByteString -> backend
              -> BSL.ByteString
buildOnClause (Condition f) prefix backend =
  let cond_ = uncurry formatQuery $ f prefix backend
  in if BSL.null cond_ then " ON 1=1" else " ON " <> cond_

buildColumnList :: [Column] -> BSL.ByteString
buildColumnList = BSL.fromChunks . intersperse ", " . map (($ "") . unColumn)

buildRelationName :: RelationName -> BSL.ByteString
buildRelationName relName = BSL.fromStrict $ unRelationName relName ""

buildSelectClauses :: SelectClauses backend a -> backend -> BSL.ByteString
buildSelectClauses SelectClauses{..} backend = mconcat
  [ if null (selectGroupBy backend)
      then ""
      else " GROUP BY " <> buildColumnList (selectGroupBy backend)
  , let (descFlag, cols) = selectOrderBy backend
    in if null cols
         then ""
         else " ORDER BY "
              <> buildColumnList cols
              <> if descFlag then " DESC" else " ASC"
  , maybe "" (\n -> " LIMIT " <> BSL.pack (show n)) selectLimit
  , maybe "" (\n -> " OFFSET " <> BSL.pack (show n)) selectOffset
  ]

buildSelectRequest :: backend -> Relation backend k l a -> Condition backend a
                   -> SelectClauses backend a -> BSL.ByteString
buildSelectRequest backend Relation{..} cond clauses =
  "SELECT " <> buildColumnList (vectorToList relationIDColumns) <> ", "
            <> buildColumnList (vectorToList relationColumns)
            <> " FROM " <> buildRelationName relationName
            <> buildWhereClause cond "" backend
            <> buildSelectClauses clauses backend

buildCountRequest :: backend -> Relation backend k l a -> Condition backend a
                  -> BSL.ByteString
buildCountRequest backend Relation{..} cond =
  "SELECT COUNT(*) FROM " <> buildRelationName relationName
                          <> buildWhereClause cond "" backend

data SelectF backend a
  = forall k l b. (Storable backend k l b, FromRow backend (k :+ l) (Entity b))
    => Select (Relation backend k l b) (Condition backend b)
              (SelectClauses backend b) ([Entity b] -> a)
  | forall k l b. Storable backend k l b
    => Count (Relation backend k l b) (Condition backend b) (Integer -> a)
  | SelectGetBackend (backend -> a)
  | SelectThrowError SeakaleError
  | SelectCatchError a (SeakaleError -> a)

instance Functor (SelectF backend) where
  fmap f = \case
    Select rel cond clauses g       -> Select rel cond clauses (f . g)
    Count  rel cond         g       -> Count  rel cond         (f . g)
    SelectGetBackend g              -> SelectGetBackend (f . g)
    SelectThrowError err            -> SelectThrowError err
    SelectCatchError action handler -> SelectCatchError (f action) (f . handler)

type SelectT backend = FreeT (SelectF backend)
type Select  backend = SelectT backend Identity

class MonadSeakaleBase backend m => MonadSelect backend m where
  select :: (Storable backend k l a, FromRow backend (k :+ l) (Entity a))
         => Relation backend k l a -> Condition backend a
         -> SelectClauses backend a -> m [Entity a]
  count :: Storable backend k l a => Relation backend k l a
        -> Condition backend a -> m Integer

instance Monad m => MonadSeakaleBase backend (FreeT (SelectF backend) m) where
  getBackend        = liftF $ SelectGetBackend id
  throwSeakaleError = liftF . SelectThrowError
  catchSeakaleError action handler =
    FreeT $ return $ Free $ SelectCatchError action handler

instance Monad m => MonadSelect backend (FreeT (SelectF backend) m) where
  select rel cond clauses = liftF $ Select rel cond clauses id
  count  rel cond         = liftF $ Count  rel cond         id

instance {-# OVERLAPPABLE #-} ( MonadSelect backend m, MonadTrans t
                              , MonadSeakaleBase backend (t m) )
  => MonadSelect backend (t m) where
  select rel cond clauses = lift $ select rel cond clauses
  count  rel cond         = lift $ count  rel cond

instance Monad m => MonadSelect backend (RequestT backend m) where
  select rel cond clauses = runSelectT $ select rel cond clauses
  count  rel cond         = runSelectT $ count  rel cond

runSelectT :: Monad m => SelectT backend m a -> RequestT backend m a
runSelectT = iterTM interpreter
  where
    interpreter :: Monad m => SelectF backend (RequestT backend m a)
                -> RequestT backend m a
    interpreter = \case
      Select rel cond clauses f -> do
        backend <- getBackend
        let req = buildSelectRequest backend rel cond clauses
        (cols, rows) <- query req
        case parseRows fromRow backend cols rows of
          Left err -> throwSeakaleError $ RowParseError err
          Right xs -> f xs

      Count rel cond f -> do
        backend <- getBackend
        let req = buildCountRequest backend rel cond
        (cols, rows) <- query req
        case parseRows fromRow backend cols rows of
          Left  err -> throwSeakaleError $ RowParseError err
          Right [x] -> f x
          Right _   ->
            throwSeakaleError $ RowParseError "Non-unique response to count"

      SelectGetBackend f              -> getBackend >>= f
      SelectThrowError err            -> throwSeakaleError err
      SelectCatchError action handler -> catchSeakaleError action handler

runSelect :: Select backend a -> Request backend a
runSelect = runSelectT

data UpdateSetter backend a
  = forall n. UpdateSetter (backend -> Vector n (Column, BS.ByteString))

instance Monoid (UpdateSetter backend a) where
  mempty = UpdateSetter $ const Nil
  mappend (UpdateSetter f) (UpdateSetter g) =
    UpdateSetter $ \backend -> f backend `vappend` g backend

eqUpdateSetter :: backend -> UpdateSetter backend a -> UpdateSetter backend a
               -> Bool
eqUpdateSetter backend (UpdateSetter f) (UpdateSetter g) =
  f backend `eqVector` g backend

data StoreF backend a
  = forall k l b. ( Storable backend k l b, ToRow backend l b
                  , FromRow backend k (EntityID b) )
    => Insert [b] ([EntityID b] -> a)
  | forall k l b. Storable backend k l b
    => Update (UpdateSetter backend b) (Condition backend b) (Integer -> a)
  | forall k l b. Storable backend k l b
    => Delete (Condition backend b) (Integer -> a)

instance Functor (StoreF backend) where
  fmap f = \case
    Insert dat         g -> Insert dat         (f . g)
    Update setter cond g -> Update setter cond (f . g)
    Delete        cond g -> Delete        cond (f . g)

type StoreT backend m = FreeT (StoreF backend) (SelectT backend m)
type Store  backend   = StoreT backend Identity

class MonadSelect backend m => MonadStore backend m where
  insert :: ( Storable backend k l b, ToRow backend l b
            , FromRow backend k (EntityID b) ) => [b] -> m [EntityID b]
  update :: Storable backend k l a => UpdateSetter backend a
         -> Condition backend a -> m Integer
  delete :: Storable backend k l a => Condition backend a -> m Integer

instance MonadSeakaleBase backend m
  => MonadSeakaleBase backend (FreeT (StoreF backend) m) where
  getBackend        = lift getBackend
  throwSeakaleError = lift . throwSeakaleError

  FreeT m `catchSeakaleError` handler = FreeT $
    liftM (fmap (`catchSeakaleError` handler)) m
      `catchSeakaleError` (runFreeT . handler)

instance MonadSelect backend m
  => MonadStore backend (FreeT (StoreF backend) m) where
  insert dat         = liftF $ Insert dat         id
  update setter cond = liftF $ Update setter cond id
  delete        cond = liftF $ Delete        cond id

instance {-# OVERLAPPABLE #-} ( MonadStore backend m, MonadTrans t
                              , MonadSeakaleBase backend (t m) )
  => MonadStore backend (t m) where
  insert dat         = lift $ insert dat
  update setter cond = lift $ update setter cond
  delete cond        = lift $ delete cond

instance Monad m => MonadStore backend (RequestT backend m) where
  insert             = runStoreT . insert
  update setter cond = runStoreT $ update setter cond
  delete             = runStoreT . delete

buildInsertRequest :: Relation backend k l a -> [QueryData l] -> BSL.ByteString
buildInsertRequest Relation{..} dat =
    let before  = Plain ("INSERT INTO "
                         <> BSL.toStrict (buildRelationName relationName)
                         <> " ("
                         <> BSL.toStrict (buildColumnList
                                          (vectorToList relationColumns))
                         <> ") VALUES ")
                        EmptyQuery
        between = buildBetween "(" ")" relationColumns
        after   = Plain (" RETURNING "
                         <> BSL.toStrict (buildColumnList
                                         (vectorToList relationIDColumns)))
                        EmptyQuery
        q = RepeatQuery before between ", " after

    in formatMany q Nil Nil dat

  where
    buildBetween :: BS.ByteString -> BS.ByteString -> Vector n a -> Query n
    buildBetween prefix suffix = \case
      Nil       -> Plain suffix EmptyQuery
      Cons _ xs -> Plain prefix $ Hole $ buildBetween ", " suffix xs

buildSetter :: backend -> UpdateSetter backend a -> BSL.ByteString
buildSetter backend (UpdateSetter f) =
  let pairs = vectorToList $ f backend
  in BSL.fromChunks $ intersperse ", " $
       map (\(col, value) -> unColumn col "" <> " = " <> value) pairs

buildUpdateRequest :: backend -> Relation backend k l a
                   -> UpdateSetter backend a -> Condition backend a
                   -> BSL.ByteString
buildUpdateRequest backend Relation{..} setter cond =
  "UPDATE " <> buildRelationName relationName <> " SET "
            <> buildSetter backend setter
            <> buildWhereClause cond "" backend

buildDeleteRequest :: backend -> Relation backend k l a -> Condition backend a
                   -> BSL.ByteString
buildDeleteRequest backend Relation{..} cond =
  "DELETE FROM " <> buildRelationName relationName
                 <> buildWhereClause cond "" backend

runStoreT :: forall backend m a. Monad m
          => StoreT backend m a -> RequestT backend m a
runStoreT = iterT interpreter . hoistFreeT runSelectT
  where
    interpreter :: Monad m => StoreF backend (RequestT backend m a)
                -> RequestT backend m a
    interpreter = \case
      Insert dat f -> _insert dat f
      Update setter cond f -> do
        backend <- getBackend
        let req = buildUpdateRequest backend (relation backend) setter cond
        f =<< execute req
      Delete cond f -> do
        backend <- getBackend
        let req = buildDeleteRequest backend (relation backend) cond
        f =<< execute req

    _insert :: forall k l b. ( Storable backend k l b, ToRow backend l b
                             , FromRow backend k (EntityID b) )
            => [b] -> ([EntityID b] -> RequestT backend m a)
            -> RequestT backend m a
    _insert dat f = do
        backend <- getBackend
        let req = buildInsertRequest
                    (relation backend :: Relation backend k l b)
                    (map (toRow backend) dat)
        (cols, rows) <- query req
        case parseRows fromRow backend cols rows of
          Left err -> throwSeakaleError $ RowParseError err
          Right xs -> f xs

runStore :: Store backend a -> Request backend a
runStore = runStoreT
