module Database.Seakale.Storable.Internal where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Free

import           Data.List hiding (insert, delete)
import           Data.Monoid
import           Data.String
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Database.Seakale.Types
import           Database.Seakale.Request.Internal

data Relation backend k l a = Relation
  { relationName      :: RelationName
  , relationIDColumns :: Vector k Column
  , relationColumns   :: Vector l Column
  }

newtype RelationName
  = RelationName { unRelationName :: BS.ByteString -> BS.ByteString }

instance IsString RelationName where
  fromString str = RelationName $ \prefix ->
    if BS.null prefix
      then fromString str
      else fromString str <> " AS " <> prefix

newtype Column = Column { unColumn :: BS.ByteString -> BS.ByteString }

instance IsString Column where
  fromString str = Column $ \prefix ->
    if BS.null prefix then fromString str else prefix <> "." <> fromString str

class Storable backend (k :: Nat) (l :: Nat) a | a -> k, a -> l where
  data EntityID a :: *
  relation :: Relation backend k l a

data Condition backend a
  = forall n. Condition (BS.ByteString -> backend -> (Query n, QueryData n))

instance Monoid (Condition backend a) where
  mempty = Condition $ \_ _ -> (EmptyQuery, Nil)
  mappend = combineConditions "AND"

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

buildCondition :: BS.ByteString -> Vector n Column
               -> (backend -> QueryData n) -> Condition backend a
buildCondition op vec dat =
    Condition $ \prefix backend ->
      (go id (fmap (($ prefix) . unColumn) vec), dat backend)
  where
    go :: (forall m. Query m -> Query m) -> Vector n BS.ByteString -> Query n
    go _ Nil = EmptyQuery
    go f (Cons x xs) =
      f $ Plain (x <> " " <> op <> " ") $ Hole $ go (Plain " AND ") xs

unsafeCastCondition :: Condition backend a -> Condition backend b
unsafeCastCondition (Condition f) = Condition f

data SelectClauses backend a = SelectClauses
  { selectGroupBy :: [Column]
  , selectOrderBy :: (Bool, [Column])
  , selectLimit   :: Maybe Int
  , selectOffset  :: Maybe Int
  }

instance Monoid (SelectClauses backend a) where
  mempty = SelectClauses
    { selectGroupBy = []
    , selectOrderBy = (False, [])
    , selectLimit   = Nothing
    , selectOffset  = Nothing
    }
  mappend sc1 sc2 = SelectClauses
    { selectGroupBy = selectGroupBy sc1 ++ selectGroupBy sc2
    , selectOrderBy = let (desc1, cols1) = selectOrderBy sc1
                          (desc2, cols2) = selectOrderBy sc2
                      in (desc1 || desc2, cols1 ++ cols2)
    , selectLimit   = maybe (selectLimit sc1) Just (selectLimit sc2)
    , selectOffset  = maybe (selectOffset sc1) Just (selectOffset sc2)
    }

unsafeCastSelectClauses :: SelectClauses backend a -> SelectClauses backend b
unsafeCastSelectClauses SelectClauses{..} = SelectClauses{..}

buildWhereClause :: backend -> Condition backend a -> BSL.ByteString
buildWhereClause backend (Condition f) =
  let cond_ = uncurry formatQuery $ f "" backend
  in if BSL.null cond_ then "" else " WHERE " <> cond_

buildColumnList :: [Column] -> BSL.ByteString
buildColumnList = BSL.fromChunks . intersperse ", " . map (($ "") . unColumn)

buildRelationName :: RelationName -> BSL.ByteString
buildRelationName relName = BSL.fromStrict $ unRelationName relName ""

buildSelectClauses :: SelectClauses backend a -> BSL.ByteString
buildSelectClauses SelectClauses{..} = mconcat
  [ if null selectGroupBy
      then ""
      else " GROUP BY " <> buildColumnList selectGroupBy
  , let (descFlag, cols) = selectOrderBy
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
            <> buildWhereClause backend cond <> buildSelectClauses clauses

data SelectF backend a
  = forall k l b. Select (Relation backend k l b) (Condition backend b)
                         (SelectClauses backend b)
                         (([ColumnInfo backend], [Row backend]) -> a)
  | SelectThrowError SeakaleError
  | SelectGetBackend (backend -> a)

instance Functor (SelectF backend) where
  fmap f = \case
    Select rel cond clauses g -> Select rel cond clauses (f . g)
    SelectThrowError err      -> SelectThrowError err
    SelectGetBackend g        -> SelectGetBackend (f . g)

type SelectT backend = FreeT (SelectF backend)
type Select  backend = SelectT backend Identity

class MonadSeakaleBase backend m => MonadSelect backend m where
  select :: Relation backend k l a -> Condition backend a
         -> SelectClauses backend a -> m ([ColumnInfo backend], [Row backend])

instance Monad m => MonadSeakaleBase backend (FreeT (SelectF backend) m) where
  throwSeakaleError = liftF . SelectThrowError
  getBackend        = liftF $ SelectGetBackend id

instance Monad m => MonadSelect backend (FreeT (SelectF backend) m) where
  select rel cond clauses = liftF $ Select rel cond clauses id

instance {-# OVERLAPPABLE #-} ( MonadSelect backend m, MonadTrans t
                              , Monad (t m) )
  => MonadSelect backend (t m) where
  select rel cond clauses = lift $ select rel cond clauses

instance Monad m => MonadSelect backend (RequestT backend m) where
  select rel cond clauses = runSelectT $ select rel cond clauses

runSelectT :: Monad m => SelectT backend m a -> RequestT backend m a
runSelectT = iterTM interpreter
  where
    interpreter :: Monad m => SelectF backend (RequestT backend m a)
                -> RequestT backend m a
    interpreter = \case
      Select rel cond clauses f -> do
        backend <- getBackend
        let req = buildSelectRequest backend rel cond clauses
        f =<< query req
      SelectThrowError err -> throwSeakaleError err
      SelectGetBackend f   -> getBackend >>= f

data UpdateSetter backend a
  = forall n. UpdateSetter (Vector n Column) (backend -> QueryData n)

instance Monoid (UpdateSetter backend a) where
  mempty = UpdateSetter Nil (const Nil)
  mappend (UpdateSetter cols1 dat1) (UpdateSetter cols2 dat2) =
    UpdateSetter (cols1 `vappend` cols2)
                 (\backend -> dat1 backend `vappend` dat2 backend)

data StoreF backend a
  = forall k l b. Insert (Relation backend k l b) [QueryData l]
                         (([ColumnInfo backend], [Row backend]) -> a)
  | forall k l b. Update (Relation backend k l b) (UpdateSetter backend b)
                         (Condition backend b) (Integer -> a)
  | forall k l b. Delete (Relation backend k l b) (Condition backend b)
                         (Integer -> a)

instance Functor (StoreF backend) where
  fmap f = \case
    Insert rel dat         g -> Insert rel dat         (f . g)
    Update rel setter cond g -> Update rel setter cond (f . g)
    Delete rel        cond g -> Delete rel        cond (f . g)

type StoreT backend m = FreeT (StoreF backend) (SelectT backend m)
type Store  backend   = StoreT backend (Select backend)

class MonadSelect backend m => MonadStore backend m where
  insert :: Relation backend k l a -> [QueryData l]
         -> m ([ColumnInfo backend], [Row backend])
  update :: Relation backend k l a -> UpdateSetter backend a
         -> Condition backend a -> m Integer
  delete :: Relation backend k l a -> Condition backend a -> m Integer

instance MonadSelect backend m
  => MonadStore backend (FreeT (StoreF backend) m) where
  insert rel dat         = liftF $ Insert rel dat         id
  update rel setter cond = liftF $ Update rel setter cond id
  delete rel        cond = liftF $ Delete rel        cond id

instance {-# OVERLAPPABLE #-} ( MonadStore backend m, MonadTrans t
                              , Monad (t m) )
  => MonadStore backend (t m) where
  insert rel dat         = lift $ insert rel dat
  update rel setter cond = lift $ update rel setter cond
  delete rel cond        = lift $ delete rel cond

instance Monad m => MonadStore backend (RequestT backend m) where
  insert rel dat         = runStoreT $ insert rel dat
  update rel setter cond = runStoreT $ update rel setter cond
  delete rel cond        = runStoreT $ delete rel cond

buildInsertRequest :: Relation backend k l a -> [QueryData l] -> BSL.ByteString
buildInsertRequest Relation{..} dat =
    let before  = Plain ("INSERT INTO "
                         <> BSL.toStrict (buildRelationName relationName)
                         <> " ("
                         <> BSL.toStrict (buildColumnList
                                          (vectorToList relationColumns))
                         <> ") VALUES")
                        EmptyQuery
        between = buildBetween " (" ")" relationColumns
        after   = Plain (" RETURNING "
                         <> BSL.toStrict (buildColumnList
                                          (vectorToList relationIDColumns)))
                        EmptyQuery
        q = RepeatQuery before between after

    in formatMany q Nil Nil dat

  where
    buildBetween :: BS.ByteString -> BS.ByteString -> Vector n a -> Query n
    buildBetween prefix suffix = \case
      Nil       -> Plain suffix EmptyQuery
      Cons _ xs -> Plain prefix $ Hole $ buildBetween ", " suffix xs

buildSetter :: backend -> UpdateSetter backend a -> BSL.ByteString
buildSetter backend (UpdateSetter cols values) =
  let pairs = zip (vectorToList cols) (vectorToList (values backend))
  in BSL.fromChunks $ intersperse ", " $
       map (\(col, value) -> unColumn col "" <> " = " <> value) pairs

buildUpdateRequest :: backend -> Relation backend k l a
                   -> UpdateSetter backend a -> Condition backend a
                   -> BSL.ByteString
buildUpdateRequest backend Relation{..} setter cond =
  "UPDATE " <> buildRelationName relationName <> " SET "
            <> buildSetter backend setter
            <> buildWhereClause backend cond

buildDeleteRequest :: backend -> Relation backend k l a -> Condition backend a
                   -> BSL.ByteString
buildDeleteRequest backend Relation{..} cond =
  "DELETE FROM " <> buildRelationName relationName
                 <> buildWhereClause backend cond

runStoreT :: Monad m => StoreT backend m a -> RequestT backend m a
runStoreT = iterT interpreter . hoistFreeT runSelectT
  where
    interpreter :: Monad m => StoreF backend (RequestT backend m a)
                -> RequestT backend m a
    interpreter = \case
      Insert rel dat f -> do
        let req = buildInsertRequest rel dat
        f =<< query req
      Update rel setter cond f -> do
        backend <- getBackend
        let req = buildUpdateRequest backend rel setter cond
        f =<< execute req
      Delete rel cond f -> do
        backend <- getBackend
        let req = buildDeleteRequest backend rel cond
        f =<< execute req
