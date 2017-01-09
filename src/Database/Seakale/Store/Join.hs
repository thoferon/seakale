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
  , JoinRelation
  , selectJoin
  , selectJoin_
  , countJoin
  , leftJoin
  , leftJoin_
  , rightJoin
  , rightJoin_
  , innerJoin
  , innerJoin_
  , fullJoin
  , fullJoin_
  ) where

import           GHC.Generics

import           Data.Monoid
import           Data.Typeable
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

data LeftJoin a b = LeftJoin a (Maybe b) deriving (Show, Eq, Generic)

instance ( Storable backend k l a, Storable backend i j b
        , (k :+ i) ~ g, (l :+ j) ~ h, Typeable g, Typeable h )
  => Storable backend g h (LeftJoin a b) where
  data EntityID (LeftJoin a b) = LeftJoinID (EntityID a) (Maybe (EntityID b))
  relation = leftJoin_ mempty

instance (FromRow backend k a, FromRow backend l (Maybe b), (k :+ l) ~ i)
  => FromRow backend i (LeftJoin a b) where
  fromRow = LeftJoin `pmap` fromRow `papply` fromRow

instance ( FromRow backend k (EntityID a)
         , FromRow backend l (Maybe (EntityID b)) , (k :+ l) ~ i )
  => FromRow backend i (EntityID (LeftJoin a b)) where
  fromRow = LeftJoinID `pmap` fromRow `papply` fromRow

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (LeftJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (LeftJoin a b))

data RightJoin a b = RightJoin (Maybe a) b deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
         , (k :+ i) ~ g, (l :+ j) ~ h, Typeable g, Typeable h )
  => Storable backend g h (RightJoin a b) where
  data EntityID (RightJoin a b) = RightJoinID (Maybe (EntityID a)) (EntityID b)
  relation = rightJoin_ mempty

instance (FromRow backend k (Maybe a), FromRow backend l b, (k :+ l) ~ i)
  => FromRow backend i (RightJoin a b) where
  fromRow = RightJoin `pmap` fromRow `papply` fromRow

instance ( FromRow backend k (Maybe (EntityID a))
         , FromRow backend l (EntityID b) , (k :+ l) ~ i )
  => FromRow backend i (EntityID (RightJoin a b)) where
  fromRow = RightJoinID `pmap` fromRow `papply` fromRow

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (RightJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (RightJoin a b))

data InnerJoin a b = InnerJoin a b deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
         , (k :+ i) ~ g, (l :+ j) ~ h, Typeable g, Typeable h )
  => Storable backend g h (InnerJoin a b) where
  data EntityID (InnerJoin a b) = InnerJoinID (EntityID a) (EntityID b)
  relation = innerJoin_ mempty

instance (FromRow backend k a, FromRow backend l b, (k :+ l) ~ i)
  => FromRow backend i (InnerJoin a b) where
  fromRow = InnerJoin `pmap` fromRow `papply` fromRow

instance ( FromRow backend k (EntityID a), FromRow backend l (EntityID b)
         , (k :+ l) ~ i )
  => FromRow backend i (EntityID (InnerJoin a b)) where
  fromRow = InnerJoinID `pmap` fromRow `papply` fromRow

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (InnerJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (InnerJoin a b))

data FullJoin a b = FullJoin (Maybe a) (Maybe b) deriving (Show, Eq)

instance ( Storable backend k l a, Storable backend i j b
        , (k :+ i) ~ g, (l :+ j) ~ h, Typeable g, Typeable h )
  => Storable backend g h (FullJoin a b) where
  data EntityID (FullJoin a b)
    = FullJoinID (Maybe (EntityID a)) (Maybe (EntityID b))
  relation = fullJoin_ mempty

instance ( FromRow backend k (Maybe a), FromRow backend l (Maybe b)
         , (k :+ l) ~ i )
  => FromRow backend i (FullJoin a b) where
  fromRow = FullJoin `pmap` fromRow `papply` fromRow

instance ( FromRow backend k (Maybe (EntityID a))
         , FromRow backend l (Maybe (EntityID b)) , (k :+ l) ~ i )
  => FromRow backend i (EntityID (FullJoin a b)) where
  fromRow = FullJoinID `pmap` fromRow `papply` fromRow

deriving instance (Show (EntityID a), Show (EntityID b))
  => Show (EntityID (FullJoin a b))
deriving instance (Eq (EntityID a), Eq (EntityID b))
  => Eq (EntityID (FullJoin a b))

type JoinRelation backend k l a = backend -> Relation backend k l a

-- | Send a @SELECT@ query on a @JOIN@.
selectJoin :: ( MonadSelect backend m, Storable backend k l (f a b)
              , FromRow backend (k :+ l) (Entity (f a b)) )
           => JoinRelation backend k l (f a b)
           -> Condition backend (f a b) -> SelectClauses backend (f a b)
           -> m [Entity (f a b)]
selectJoin jrel cond clauses = do
  backend <- getBackend
  I.select (jrel backend) cond clauses

selectJoin_ :: ( MonadSelect backend m, Storable backend k l (f a b)
               , FromRow backend (k :+ l) (Entity (f a b)) )
            => JoinRelation backend k l (f a b)
            -> Condition backend (f a b) -> m [Entity (f a b)]
selectJoin_ jrel cond = selectJoin jrel cond mempty

countJoin :: (MonadSelect backend m, Storable backend k l (f a b))
          => JoinRelation backend k l (f a b)
          -> Condition backend (f a b) -> m Integer
countJoin jrel cond = do
  backend <- getBackend
  I.count (jrel backend) cond

leftJoin :: JoinRelation backend k l a -> JoinRelation backend i j b
         -> Condition backend (LeftJoin a b)
         -> JoinRelation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin = mkJoin "LEFT JOIN"

leftJoin_ :: (Storable backend k l a, Storable backend i j b)
          => Condition backend (LeftJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin_ = leftJoin relation relation

rightJoin :: JoinRelation backend k l a -> JoinRelation backend i j b
          -> Condition backend (RightJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (RightJoin a b)
rightJoin = mkJoin "RIGHT JOIN"

rightJoin_ :: (Storable backend k l a, Storable backend i j b)
           => Condition backend (RightJoin a b)
           -> JoinRelation backend (k :+ i) (l :+ j) (RightJoin a b)
rightJoin_ = rightJoin relation relation

innerJoin :: JoinRelation backend k l a -> JoinRelation backend i j b
          -> Condition backend (InnerJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (InnerJoin a b)
innerJoin = mkJoin "INNER JOIN"

innerJoin_ :: (Storable backend k l a, Storable backend i j b)
           => Condition backend (InnerJoin a b)
           -> JoinRelation backend (k :+ i) (l :+ j) (InnerJoin a b)
innerJoin_ = innerJoin relation relation

fullJoin :: JoinRelation backend k l a -> JoinRelation backend i j b
         -> Condition backend (FullJoin a b)
         -> JoinRelation backend (k :+ i) (l :+ j) (FullJoin a b)
fullJoin = mkJoin "FULL JOIN"

fullJoin_ :: (Storable backend k l a, Storable backend i j b)
          => Condition backend (FullJoin a b)
          -> JoinRelation backend (k :+ i) (l :+ j) (FullJoin a b)
fullJoin_ = fullJoin relation relation

mkJoin :: BS.ByteString
       -> JoinRelation backend k l a -> JoinRelation backend i j b
       -> Condition backend (f a b)
       -> JoinRelation backend (k :+ i) (l :+ j) (f a b)
mkJoin joinStmt jrelA jrelB cond backend =
  let relA = jrelA backend
      relB = jrelB backend
  in combineRelations joinStmt backend relA relB cond

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
