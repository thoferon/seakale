module Database.Seakale.Storable
  ( Entity(..)
  , Storable(..)
  , MonadSelect
  , Relation(..)
  , RelationName(..)
  , Column(..)
  , Property(..)
  , Condition
  , SelectClauses
  , groupBy
  , asc
  , desc
  , limit
  , offset
  , (==.)
  , (/=.)
  , (<=.)
  , (<.)
  , (>=.)
  , (>.)
  , (&&.)
  , (||.)
  ) where

import Database.Seakale.FromRow
import Database.Seakale.ToRow
import Database.Seakale.Types

import Database.Seakale.Storable.Internal

data Entity a = Entity (EntityID a) a

{-
data InnerJoin a b = InnerJoin a b

innerJoin :: Relation backend k l a -> Relation backend i j b
          -> Relation backend (k :+ i) (l :+ j) (InnerJoin a b)
innerJoin = combineRelations " INNER JOIN "

data LeftJoin a b = LeftJoin a (Maybe b)

leftJoin :: Relation backend k l a -> Relation backend i j b
         -> Relation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin = combineRelations " LEFT JOIN "

data RightJoin a b = RightJoin (Maybe a) b

rightJoin :: Relation backend k l a -> Relation backend i j b
          -> Relation backend (k :+ i) (l :+ j) (RightJoin a b)
rightJoin = combineRelations " RIGHT JOIN "

data FullJoin a b
  = FullJoinBoth a b
  | FullJoinLeft a
  | FullJoinRight b

fullJoin :: Relation backend k l a -> Relation backend i j b
          -> Relation backend (k :+ i) (l :+ j) (FullJoin a b)
fullJoin = combineRelations " FULL JOIN "
-}

class Property a f | f -> a where
  toColumns :: f n b -> Vector n Column

(==.) :: (Property a f, ToRow backend n b) => f n b -> b -> Condition backend a
(==.) cols vals =
  buildCondition "=" (toColumns cols) (\backend -> toRow backend vals)

(/=.) :: (Property a f, ToRow backend n b) => f n b -> b -> Condition backend a
(/=.) cols vals =
  buildCondition "<>" (toColumns cols) (\backend -> toRow backend vals)

(<=.) :: (Property a f, ToRow backend n b) => f n b -> b -> Condition backend a
(<=.) cols vals =
  buildCondition "<=" (toColumns cols) (\backend -> toRow backend vals)

(<.) :: (Property a f, ToRow backend n b) => f n b -> b -> Condition backend a
(<.) cols vals =
  buildCondition "<" (toColumns cols) (\backend -> toRow backend vals)

(>=.) :: (Property a f, ToRow backend n b) => f n b -> b -> Condition backend a
(>=.) cols vals =
  buildCondition ">=" (toColumns cols) (\backend -> toRow backend vals)

(>.) :: (Property a f, ToRow backend n b) => f n b -> b -> Condition backend a
(>.) cols vals =
  buildCondition ">" (toColumns cols) (\backend -> toRow backend vals)

(&&.) :: Condition backend a -> Condition backend a -> Condition backend a
(&&.) = mappend

(||.) :: Condition backend a -> Condition backend a -> Condition backend a
(||.) = combineConditions "OR"

groupBy :: Property a f => f n b -> SelectClauses a
groupBy prop = mempty { selectGroupBy = vectorToList (toColumns prop) }

asc :: Property a f => f n b -> SelectClauses a
asc prop = mempty { selectOrderBy = (False, vectorToList (toColumns prop)) }

desc :: Property a f => f n b -> SelectClauses a
desc prop = mempty { selectOrderBy = (True, vectorToList (toColumns prop)) }

limit :: Int -> SelectClauses a
limit n = mempty { selectLimit = Just n }

offset :: Int -> SelectClauses a
offset n = mempty { selectOffset = Just n }
