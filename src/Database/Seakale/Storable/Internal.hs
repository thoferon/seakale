{-# LANGUAGE UndecidableInstances #-}

module Database.Seakale.Storable.Internal where

import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Free

import           Data.List
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

combineRelationNames :: BS.ByteString -> RelationName -> RelationName
                     -> RelationName
combineRelationNames joinStmt l r = RelationName $ \prefix ->
  unRelationName l (prefix <> "l")
  <> joinStmt
  <> unRelationName r (prefix <> "r")

combineRelationColumns :: Vector k Column -> Vector l Column
                       -> Vector (k :+ l) Column
combineRelationColumns l r =
  fmap (\col -> Column $ unColumn col . (<> "l")) l
  `vappend`
  fmap (\col -> Column $ unColumn col . (<> "r")) r

combineRelations :: BS.ByteString -> Relation backend k l a
                 -> Relation backend i j b
                 -> Relation backend (k :+ i) (l :+ j) c
combineRelations joinStmt relA relB = Relation
  { relationName =
      combineRelationNames joinStmt (relationName relA) (relationName relB)
  , relationIDColumns =
      combineRelationColumns (relationIDColumns relA) (relationIDColumns relB)
  , relationColumns =
      combineRelationColumns (relationColumns relA) (relationColumns relB)
  }

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

data SelectClauses a = SelectClauses
  { selectGroupBy :: [Column]
  , selectOrderBy :: (Bool, [Column])
  , selectLimit   :: Maybe Int
  , selectOffset  :: Maybe Int
  }

instance Monoid (SelectClauses a) where
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

buildWhereClause :: backend -> Condition backend a -> BSL.ByteString
buildWhereClause backend (Condition f) =
  let cond_ = uncurry formatQuery $ f "" backend
  in if BSL.null cond_ then "" else " WHERE " <> cond_

buildColumnList :: [Column] -> BSL.ByteString
buildColumnList = BSL.fromChunks . intersperse ", " . map (($ "") . unColumn)

buildRelationName :: RelationName -> BSL.ByteString
buildRelationName relName = BSL.fromStrict $ unRelationName relName ""

buildSelectClauses :: SelectClauses a -> BSL.ByteString
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
                   -> SelectClauses a -> BSL.ByteString
buildSelectRequest backend Relation{..} cond clauses =
  "SELECT " <> buildColumnList (vectorToList relationIDColumns) <> ", "
            <> buildColumnList (vectorToList relationColumns)
            <> " FROM " <> buildRelationName relationName
            <> buildWhereClause backend cond <> buildSelectClauses clauses

data SelectF backend a
  = forall k l b. Select (Relation backend k l b) (Condition backend b)
                         (SelectClauses b)
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
  select :: Relation backend k l a -> Condition backend a -> SelectClauses a
         -> m ([ColumnInfo backend], [Row backend])

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

data UpdateSetter a = UpdateSetter Column BS.ByteString

data StorableF backend a
  = forall k l b. Insert (Relation backend k l b) [QueryData l]
                         (([ColumnInfo backend], [Row backend]) -> a)
  | forall k l b. Update (Relation backend k l b) [UpdateSetter b]
                         (Condition backend b) (Integer -> a)
