-- | This module allows to make @SELECT@ queries on several tables with
-- @LEFT JOIN@, @RIGHT JOIN@, @INNER JOIN@ and @FULL JOIN@. Note that they can
-- be nested.
--
-- To be able to create 'Conditon's and 'SelectClause's, 'JLeft' and 'JRight'
-- are provided to lift properties of a storable value into properties of a
-- join.

module Database.Seakale.Store.Join
  ( JoinLeftProperty(..)
  , JoinRightProperty(..)
  , EntityID(..)
  , LeftJoin(..)
  , RightJoin(..)
  , InnerJoin(..)
  , FullJoin(..)
  , selectJoin
  , selectJoin_
  , table
  , leftJoin
  , leftJoin_
  , rightJoin
  , rightJoin_
  , innerJoin
  , innerJoin_
  , fullJoin
  , fullJoin_
  ) where

import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.FromRow
import           Database.Seakale.Store
import           Database.Seakale.Types
import qualified Database.Seakale.Store.Internal as I

data JoinLeftProperty (j :: * -> * -> *) f b backend (n :: Nat) c
  = JLeft (f backend n c)
data JoinRightProperty (j :: * -> * -> *) f a backend (n :: Nat) c
  = JRight (f backend n c)

instance Property backend a f
  => Property backend (j a b) (JoinLeftProperty j f b) where
  toColumns backend (JLeft prop) =
    fmap (\col -> Column $ unColumn col . (<> "l")) (toColumns backend prop)

instance Property backend b f
  => Property backend (j a b) (JoinRightProperty j f a) where
  toColumns backend (JRight prop) =
    fmap (\col -> Column $ unColumn col . (<> "r")) (toColumns backend prop)

data LeftJoin a b = LeftJoin a (Maybe b) deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
         , FromRow backend k (EntityID a), FromRow backend l a
         , FromRow backend i (EntityID b), FromRow backend j b
         , FromRow backend i (Vector i Null)
         , FromRow backend j (Vector j Null)
        , (k :+ i) ~ g, (l :+ j) ~ h )
  => Storable backend g h (LeftJoin a b) where
  data EntityID (LeftJoin a b) = LeftJoinID (EntityID a) (Maybe (EntityID b))
  relation backend = let (rel, _, _) = leftJoin_ mempty backend in rel

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (LeftJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (LeftJoin a b))

data RightJoin a b = RightJoin (Maybe a) b deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
         , FromRow backend k (EntityID a), FromRow backend l a
         , FromRow backend i (EntityID b), FromRow backend j b
         , FromRow backend k (Vector k Null)
         , FromRow backend l (Vector l Null)
         , (k :+ i) ~ g, (l :+ j) ~ h )
  => Storable backend g h (RightJoin a b) where
  data EntityID (RightJoin a b) = RightJoinID (Maybe (EntityID a)) (EntityID b)
  relation backend = let (rel, _, _) = rightJoin_ mempty backend in rel

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (RightJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (RightJoin a b))

data InnerJoin a b = InnerJoin a b deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
         , FromRow backend k (EntityID a), FromRow backend l a
         , FromRow backend i (EntityID b), FromRow backend j b
         , (k :+ i) ~ g, (l :+ j) ~ h )
  => Storable backend g h (InnerJoin a b) where
  data EntityID (InnerJoin a b) = InnerJoinID (EntityID a) (EntityID b)
  relation backend = let (rel, _, _) = innerJoin_ mempty backend in rel

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (InnerJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (InnerJoin a b))

data FullJoin a b = FullJoin (Maybe a) (Maybe b) deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
         , FromRow backend k (EntityID a), FromRow backend l a
         , FromRow backend i (EntityID b), FromRow backend j b
         , FromRow backend k (Vector k Null)
         , FromRow backend l (Vector l Null)
         , FromRow backend i (Vector i Null)
         , FromRow backend j (Vector j Null)
        , (k :+ i) ~ g, (l :+ j) ~ h )
  => Storable backend g h (FullJoin a b) where
  data EntityID (FullJoin a b)
    = FullJoinID (Maybe (EntityID a)) (Maybe (EntityID b))
  relation backend = let (rel, _, _) = fullJoin_ mempty backend in rel

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (FullJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (FullJoin a b))

type JoinRelation backend k l a
  = backend -> ( Relation backend k l a
               , RowParser backend k (EntityID a)
               , RowParser backend l a )

-- | Send a @SELECT@ query on a @JOIN@.
selectJoin :: MonadSelect backend m
           => JoinRelation backend k l (f a b)
           -> Condition backend (f a b) -> SelectClauses backend (f a b)
           -> m [Entity (f a b)]
selectJoin jrel cond clauses = do
  backend <- getBackend
  let (rel, idParser, valParser) = jrel backend
      entParser = Entity `pmap` idParser `papply` valParser
  (cols, rows) <- I.select rel cond clauses
  case parseRows entParser backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

selectJoin_ :: MonadSelect backend m
            => JoinRelation backend k l (f a b)
            -> Condition backend (f a b) -> m [Entity (f a b)]
selectJoin_ rel cond = selectJoin rel cond mempty

leftJoin :: ( FromRow backend i (Vector i Null)
            , FromRow backend j (Vector j Null) )
         => JoinRelation backend k l a -> JoinRelation backend i j b
         -> Condition backend (LeftJoin a b)
         -> JoinRelation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin = mkJoin "LEFT JOIN"
  (\parserA parserB -> LeftJoinID `pmap` parserA `papply` maybeParser parserB)
  (\parserA parserB -> LeftJoin   `pmap` parserA `papply` maybeParser parserB)

leftJoin_ :: ( Storable backend k l a, Storable backend i j b
             , FromRow backend k (EntityID a), FromRow backend l a
             , FromRow backend i (EntityID b), FromRow backend j b
             , FromRow backend i (Vector i Null)
             , FromRow backend j (Vector j Null) )
          => Condition backend (LeftJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin_ = leftJoin table table

rightJoin :: ( FromRow backend k (Vector k Null)
             , FromRow backend l (Vector l Null) )
          => JoinRelation backend k l a -> JoinRelation backend i j b
          -> Condition backend (RightJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (RightJoin a b)
rightJoin = mkJoin "RIGHT JOIN"
  (\parserA parserB -> RightJoinID `pmap` maybeParser parserA `papply` parserB)
  (\parserA parserB -> RightJoin   `pmap` maybeParser parserA `papply` parserB)

rightJoin_ :: ( Storable backend k l a, Storable backend i j b
              , FromRow backend k (EntityID a), FromRow backend l a
              , FromRow backend i (EntityID b), FromRow backend j b
              , FromRow backend k (Vector k Null)
              , FromRow backend l (Vector l Null) )
           => Condition backend (RightJoin a b)
           -> JoinRelation backend (k :+ i) (l :+ j) (RightJoin a b)
rightJoin_ = rightJoin table table

innerJoin :: JoinRelation backend k l a -> JoinRelation backend i j b
          -> Condition backend (InnerJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (InnerJoin a b)
innerJoin = mkJoin "INNER JOIN"
  (\parserA parserB -> InnerJoinID `pmap` parserA `papply` parserB)
  (\parserA parserB -> InnerJoin   `pmap` parserA `papply` parserB)

innerJoin_ :: ( Storable backend k l a, Storable backend i j b
              , FromRow backend k (EntityID a), FromRow backend l a
              , FromRow backend i (EntityID b), FromRow backend j b )
           => Condition backend (InnerJoin a b)
           -> JoinRelation backend (k :+ i) (l :+ j) (InnerJoin a b)
innerJoin_ = innerJoin table table

fullJoin :: ( FromRow backend k (Vector k Null)
            , FromRow backend l (Vector l Null)
            , FromRow backend i (Vector i Null)
            , FromRow backend j (Vector j Null) )
         => JoinRelation backend k l a -> JoinRelation backend i j b
         -> Condition backend (FullJoin a b)
         -> JoinRelation backend (k :+ i) (l :+ j) (FullJoin a b)
fullJoin = mkJoin "FULL JOIN"
  (\parserA parserB ->
    FullJoinID `pmap` maybeParser parserA `papply` maybeParser parserB)
  (\parserA parserB ->
    FullJoin   `pmap` maybeParser parserA `papply` maybeParser parserB)

fullJoin_ :: ( Storable backend k l a, Storable backend i j b
             , FromRow backend k (EntityID a), FromRow backend l a
             , FromRow backend i (EntityID b), FromRow backend j b
             , FromRow backend k (Vector k Null)
             , FromRow backend l (Vector l Null)
             , FromRow backend i (Vector i Null)
             , FromRow backend j (Vector j Null) )
          => Condition backend (FullJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (FullJoin a b)
fullJoin_ = fullJoin table table

table :: ( Storable backend k l a , FromRow backend k (EntityID a)
         , FromRow backend l a) => JoinRelation backend k l a
table backend = (relation backend, fromRow, fromRow)

mkJoin :: BS.ByteString
       -> (RowParser backend k (EntityID a) -> RowParser backend i (EntityID b)
           -> RowParser backend (k :+ i) (EntityID (f a b)))
       -> (RowParser backend l a -> RowParser backend j b
           -> RowParser backend (l :+ j) (f a b))
       -> JoinRelation backend k l a -> JoinRelation backend i j b
       -> Condition backend (f a b)
       -> JoinRelation backend (k :+ i) (l :+ j) (f a b)
mkJoin joinStmt f g jrelA jrelB cond backend =
  let (relA, idParserA, valParserA) = jrelA backend
      (relB, idParserB, valParserB) = jrelB backend
      rel = combineRelations joinStmt backend relA relB cond
      idParser  = f idParserA  idParserB
      valParser = g valParserA valParserB
  in (rel, idParser, valParser)

combineRelationNames :: BS.ByteString -> backend -> RelationName -> RelationName
                     -> Condition backend a -> RelationName
combineRelationNames joinStmt backend l r cond = RelationName $ \prefix ->
  unRelationName l (prefix <> "l")
  <> " " <> joinStmt <> " "
  <> unRelationName r (prefix <> "r")
  <> BSL.toStrict (I.buildOnClause cond prefix backend)

combineRelationColumns :: Vector k Column -> Vector l Column
                       -> Vector (k :+ l) Column
combineRelationColumns l r =
  fmap (\col -> Column $ unColumn col . (<> "l")) l
  `vappend`
  fmap (\col -> Column $ unColumn col . (<> "r")) r

combineRelations :: BS.ByteString -> backend -> Relation backend k l a
                 -> Relation backend i j b -> Condition backend c
                 -> Relation backend (k :+ i) (l :+ j) c
combineRelations joinStmt backend relA relB cond = Relation
  { relationName =
      combineRelationNames joinStmt backend
                           (relationName relA) (relationName relB) cond
  , relationIDColumns =
      combineRelationColumns (relationIDColumns relA) (relationIDColumns relB)
  , relationColumns =
      combineRelationColumns (relationColumns relA) (relationColumns relB)
  }
