-- | This module provides functions and types to work on values instantiating
-- 'Storable'. Such values have an associated type for their ID and an
-- associated relation (table name, columns for the ID and columns for the
-- value.)
--
-- The type classes 'MonadSelect' and 'MonadStore' and provided so that code can
-- be written with types such as @MonadSelect m => Int -> m String@ ensuring
-- that the function is read-only. In 'MonadStore', all four operations
-- (@SELECT@, @INSERT@, @UPDATE@ and @DELETE#) can be done.
--
-- In order to be able to use a type with these functions, it should be made an
-- instance of 'Storable' as well as possibly an instance of 'FromRow'/'ToRow'
-- depending on what functions are called. It is also a good idea to define a
-- type specifying the properties (fields) on which we can define conditions.
-- See the demo for an example.

module Database.Seakale.Store
  ( Entity(..)
  , MonadSelect
  , MonadStore
  -- * Operations
  , select
  , select_
  , count
  , getMany
  , getMaybe
  , get
  , insertMany
  , insert
  , updateMany
  , update
  , UpdateSetter
  , (=.)
  , deleteMany
  , delete
  -- * Setup
  , Storable(..)
  , Relation(..)
  , RelationName(..)
  , Column(..)
  -- * Properties
  , Property(..)
  , EntityIDProperty(..)
  -- * SELECT clauses
  , SelectClauses
  , groupBy
  , asc
  , desc
  , limit
  , offset
  -- * Conditions
  , Condition
  , (==.)
  , (/=.)
  , (<=.)
  , (<.)
  , (>=.)
  , (>.)
  , (==#)
  , (/=#)
  , (<=#)
  , (<#)
  , (>=#)
  , (>#)
  , (==~)
  , (/=~)
  , (<=~)
  , (<~)
  , (>=~)
  , (>~)
  , (&&.)
  , (||.)
  , isNull
  , isNotNull
  , inList
  , notInList
  ) where

import           Control.Monad

import           Data.List hiding (groupBy, insert, delete)
import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.FromRow
import           Database.Seakale.ToRow
import           Database.Seakale.Types

import           Database.Seakale.Store.Internal
                   hiding (select, count, insert, update, delete)
import qualified Database.Seakale.Store.Internal as I

-- | Select all entities for the corresponding relation.
select :: ( MonadSelect backend m, Storable backend k l a
          , FromRow backend (k :+ l) (Entity a) ) => Condition backend a
       -> SelectClauses backend a -> m [Entity a]
select cond clauses = do
  backend <- getBackend
  I.select (relation backend) cond clauses

-- | Like 'select' but without any other clauses than @WHERE@.
select_ :: ( MonadSelect backend m, Storable backend k l a
          , FromRow backend (k :+ l) (Entity a) ) => Condition backend a
        -> m [Entity a]
select_ cond = select cond mempty

-- | Count the number of rows matching the conditions.
count :: (MonadSelect backend m, Storable backend k l a) => Condition backend a
      -> m Integer
count cond = do
  backend <- getBackend
  I.count (relation backend) cond

-- | Select all entities with the given IDs.
getMany :: ( MonadSelect backend m, Storable backend k l a
           , FromRow backend (k :+ l) (Entity a), ToRow backend k (EntityID a) )
        => [EntityID a] -> m [Entity a]
getMany ids = select_ $ EntityID `inList` ids

-- | Return the value corresponding to the given ID if it exists, otherwise
-- return @Nothing@.
getMaybe :: ( MonadSelect backend m, Storable backend k l a
            , FromRow backend (k :+ l) (Entity a), ToRow backend k (EntityID a)
            ) => EntityID a -> m (Maybe a)
getMaybe i =
  (fmap entityVal . listToMaybe) <$> select (EntityID ==. i) (limit 1)

-- | Return the value corresponding to the given ID if it exists, otherwise
-- throw 'EntityNotFoundError'.
get :: ( MonadSelect backend m, Storable backend k l a
       , FromRow backend (k :+ l) (Entity a), ToRow backend k (EntityID a) )
    => EntityID a -> m a
get i = maybe (throwSeakaleError EntityNotFoundError) return =<< getMaybe i

-- | Insert the given values and return their ID in the same order.
insertMany :: forall backend m k l a.
              ( MonadStore backend m, Storable backend k l a, ToRow backend l a
              , FromRow backend k (EntityID a) ) => [a] -> m [EntityID a]
insertMany = I.insert

-- | Like 'insertMany' but for only one value.
insert :: ( MonadStore backend m, Storable backend k l a, ToRow backend l a
          , FromRow backend k (EntityID a) ) => a -> m (EntityID a)
insert = fmap head . insertMany . pure

-- | Update columns on rows matching the given conditions and return the number
-- of rows affected.
updateMany :: forall backend m k l a.
              (MonadStore backend m, Storable backend k l a)
           => UpdateSetter backend a -> Condition backend a -> m Integer
updateMany setter cond = I.update setter cond

-- | Update columns on the row with the given ID.
update :: ( MonadStore backend m, Storable backend k l a
          , ToRow backend k (EntityID a) )
       => EntityID a -> UpdateSetter backend a -> m ()
update i setter = do
  n <- updateMany setter $ EntityID ==. i
  unless (n == 1) $ throwSeakaleError EntityNotFoundError

-- | Delete rows matching the given conditions.
deleteMany :: forall backend m k l a.
              (MonadStore backend m, Storable backend k l a)
           => Condition backend a -> m Integer
deleteMany = I.delete

-- | Delete the row with the given ID.
delete :: ( MonadStore backend m, Storable backend k l a
          , ToRow backend k (EntityID a) ) => EntityID a -> m ()
delete i = do
  n <- deleteMany $ EntityID ==. i
  unless (n == 1) $ throwSeakaleError EntityNotFoundError

-- | Specify that the type @f@ specify properties of @a@. These values of type
-- @f@ can then be used to create 'Condition's on type @a@. The type parameters
-- 'n' and 'b' in the class definition are, respectively, the number of rows
-- taken by this property and the associated type.
--
-- See the following example:
--
-- > data User = User
-- >   { userFirstName :: String
-- >   , userLastName  :: String
-- >   }
-- >
-- > data UserProperty b n a where
-- >   UserFirstName :: UserProperty b One String
-- >   UserLastName  :: UserProperty b One String
-- >
-- > UserFirstName ==. "Marie" &&. UserLastName ==. "Curie"
-- >   :: Condition backend User
class Property backend a f | f -> a where
  toColumns :: backend -> f backend n b -> Vector n Column

-- | Property of any value instantiating 'Storable' and selecting its ID.
-- This can be used to easily create 'Condition's on any type such as
-- @EntityID ==. UserID 42@.
data EntityIDProperty a backend :: Nat -> * -> * where
  EntityID :: forall backend k l a. Storable backend k l a
           => EntityIDProperty a backend k (EntityID a)

instance Property backend a (EntityIDProperty a) where
  toColumns backend x@EntityID = go (relation backend) x
    where
      go :: Relation backend k l a -> EntityIDProperty a backend k (EntityID a)
         -> Vector k Column
      go Relation{..} _ = relationIDColumns

(==.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(==.) prop vals =
  buildCondition "=" "IS" (flip toColumns prop) (flip toRow vals)

(/=.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(/=.) prop vals =
  buildCondition "<>" "IS NOT" (flip toColumns prop) (flip toRow vals)

(<=.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(<=.) prop vals =
  buildCondition "<=" "<=" (flip toColumns prop) (flip toRow vals)

(<.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
     -> Condition backend a
(<.) prop vals =
  buildCondition "<" "<" (flip toColumns prop) (flip toRow vals)

(>=.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(>=.) prop vals =
  buildCondition ">=" ">=" (flip toColumns prop) (flip toRow vals)

(>.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
     -> Condition backend a
(>.) cols vals =
  buildCondition ">" ">" (flip toColumns cols) (flip toRow vals)

(==#) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n b -> Condition backend a
(==#) prop1 prop2 =
  buildCondition' "=" (flip toColumns prop1) (flip toColumns prop2)

(/=#) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n b -> Condition backend a
(/=#) prop1 prop2 =
  buildCondition' "<>" (flip toColumns prop1) (flip toColumns prop2)

(<#) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n b -> Condition backend a
(<#) prop1 prop2 =
  buildCondition' "<" (flip toColumns prop1) (flip toColumns prop2)

(<=#) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n b -> Condition backend a
(<=#) prop1 prop2 =
  buildCondition' "<=" (flip toColumns prop1) (flip toColumns prop2)

(>#) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n b -> Condition backend a
(>#) prop1 prop2 =
  buildCondition' ">" (flip toColumns prop1) (flip toColumns prop2)

(>=#) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n b -> Condition backend a
(>=#) prop1 prop2 =
  buildCondition' ">=" (flip toColumns prop1) (flip toColumns prop2)

(==~) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n c -> Condition backend a
(==~) prop1 prop2 =
  buildCondition' "=" (flip toColumns prop1) (flip toColumns prop2)

(/=~) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n c -> Condition backend a
(/=~) prop1 prop2 =
  buildCondition' "<>" (flip toColumns prop1) (flip toColumns prop2)

(<~) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n c -> Condition backend a
(<~) prop1 prop2 =
  buildCondition' "<" (flip toColumns prop1) (flip toColumns prop2)

(<=~) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n c -> Condition backend a
(<=~) prop1 prop2 =
  buildCondition' "<=" (flip toColumns prop1) (flip toColumns prop2)

(>~) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n c -> Condition backend a
(>~) prop1 prop2 =
  buildCondition' ">" (flip toColumns prop1) (flip toColumns prop2)

(>=~) :: (Property backend a f, Property backend a g)
      => f backend n b -> g backend n c -> Condition backend a
(>=~) prop1 prop2 =
  buildCondition' ">=" (flip toColumns prop1) (flip toColumns prop2)

(&&.) :: Condition backend a -> Condition backend a -> Condition backend a
(&&.) = mappend

(||.) :: Condition backend a -> Condition backend a -> Condition backend a
(||.) = combineConditions "OR"

infix 4 ==., /=., <=., <., >=., >.
infix 4 ==#, /=#, <=#, <#, >=#, >#
infix 4 ==~, /=~, <=~, <~, >=~, >~
infixr 2 &&., ||.

isNull :: Property backend a f => f backend n b -> Condition backend a
isNull prop = Condition $ \prefix backend ->
  let bs = mconcat . intersperse " AND "
           . map (\col -> unColumn col prefix <> " IS NULL")
           . vectorToList $ toColumns backend prop
  in (Plain bs EmptyQuery, Nil)

isNotNull :: Property backend a f => f backend n b -> Condition backend a
isNotNull prop = Condition $ \prefix backend ->
  let bs = mconcat . intersperse " AND "
           . map (\col -> unColumn col prefix <> " IS NOT NULL")
           . vectorToList $ toColumns backend prop
  in (Plain bs EmptyQuery, Nil)

listHelper :: (Property backend a f, ToRow backend n b) => BS.ByteString
           -> f backend n b -> [b] -> Condition backend a
listHelper op prop values =
  let step value (suffix, (Condition f)) =
        (", ",) $ Condition $ \prefix backend ->
          let (req, dat) = f prefix backend
              valueList = mconcat $ intersperse ", " $ map (fromMaybe "NULL") $
                            vectorToList $ toRow backend value
          in ( Hole (Plain suffix req)
             , Cons (Just ("(" <> valueList <> ")")) dat )

      (_, cond) = foldr step ("", mempty) values

  in case cond of
       Condition f -> Condition $ \prefix backend ->
         let (req, dat) = f prefix backend
             colList = mconcat $ intersperse ", " $ map (flip unColumn prefix) $
                         vectorToList $ toColumns backend prop
             req' =  Plain ("(" <> colList <> ") " <> op <> " (") req
                          `qappendZero` Plain ")" EmptyQuery
         in (req', dat)

inList :: (Property backend a f, ToRow backend n b) => f backend n b -> [b]
       -> Condition backend a
inList _ [] = Condition $ \_ _ -> (Plain "1=0" EmptyQuery, Nil)
inList prop values = listHelper "IN" prop values

notInList :: (Property backend a f, ToRow backend n b) => f backend n b -> [b]
          -> Condition backend a
notInList _ [] = Condition $ \_ _ -> (Plain "1=1" EmptyQuery, Nil)
notInList prop values = listHelper "NOT IN" prop values

groupBy :: Property backend a f => f backend n b -> SelectClauses backend a
groupBy prop = mempty { selectGroupBy = vectorToList . flip toColumns prop }

asc :: Property backend a f => f backend n b -> SelectClauses backend a
asc prop =
  let f backend = map (,Asc) $ vectorToList (toColumns backend prop)
  in mempty { selectOrderBy = f }

desc :: Property backend a f => f backend n b -> SelectClauses backend a
desc prop =
  let f backend = map (,Desc) $ vectorToList (toColumns backend prop)
  in mempty { selectOrderBy = f }

limit :: Int -> SelectClauses backend a
limit n = mempty { selectLimit = Just n }

offset :: Int -> SelectClauses backend a
offset n = mempty { selectOffset = Just n }

(=.) :: (Property backend a f, ToRow backend n b)
     => f backend n b -> b -> UpdateSetter backend a
(=.) col value = UpdateSetter $ \backend ->
  vzip (toColumns backend col) (fmap (fromMaybe "NULL") $ toRow backend value)
