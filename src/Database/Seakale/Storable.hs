module Database.Seakale.Storable
  ( Entity(..)
  , Storable(..)
  , MonadSelect
  , select
  , select_
  , getMany
  , getMaybe
  , get
  , createMany
  , create
  , updateMany
  , update
  , deleteMany
  , delete
  , Relation(..)
  , RelationName(..)
  , Column(..)
  , Property(..)
  , SelectClauses
  , groupBy
  , asc
  , desc
  , limit
  , offset
  , Condition
  , EntityIDProperty(..)
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
  , UpdateSetter
  , (=.)
  ) where

import           Control.Monad

import           Data.List hiding (groupBy, delete)
import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.FromRow
import           Database.Seakale.ToRow
import           Database.Seakale.Types

import           Database.Seakale.Storable.Internal
                   hiding (select, insert, update, delete)
import qualified Database.Seakale.Storable.Internal as I

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

select :: ( MonadSelect backend m, Storable backend k l a
          , FromRow backend (k :+ l) (Entity a) ) => Condition backend a
       -> SelectClauses backend a -> m [Entity a]
select cond clauses = do
  backend <- getBackend
  (cols, rows) <- I.select (relation backend) cond clauses
  case parseRows fromRow backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

select_ :: ( MonadSelect backend m, Storable backend k l a
          , FromRow backend (k :+ l) (Entity a) ) => Condition backend a
        -> m [Entity a]
select_ cond = select cond mempty

data EntityIDProperty a backend :: Nat -> * -> * where
  EntityID :: forall backend k l a. Storable backend k l a
           => EntityIDProperty a backend k (EntityID a)

instance Property backend a (EntityIDProperty a) where
  toColumns backend x@EntityID = go (relation backend) x
    where
      go :: Relation backend k l a -> EntityIDProperty a backend k (EntityID a)
         -> Vector k Column
      go Relation{..} _ = relationIDColumns

getMany :: ( MonadSelect backend m, Storable backend k l a
           , FromRow backend (k :+ l) (Entity a), ToRow backend k (EntityID a) )
        => [EntityID a] -> m [Entity a]
getMany ids = select_ $ EntityID `inList` ids

getMaybe :: ( MonadSelect backend m, Storable backend k l a
            , FromRow backend (k :+ l) (Entity a), ToRow backend k (EntityID a)
            ) => EntityID a -> m (Maybe a)
getMaybe i =
  (fmap entityVal . listToMaybe) <$> select (EntityID ==. i) (limit 1)

get :: ( MonadSelect backend m, Storable backend k l a
       , FromRow backend (k :+ l) (Entity a), ToRow backend k (EntityID a) )
    => EntityID a -> m a
get i = maybe (throwSeakaleError EntityNotFoundError) return =<< getMaybe i

createMany :: forall backend m k l a.
              ( MonadStore backend m, Storable backend k l a, ToRow backend l a
              , FromRow backend k (EntityID a) ) => [a] -> m [EntityID a]
createMany values = do
  backend <- getBackend
  let rel = relation backend :: Relation backend k l a
      dat = map (toRow backend) values
  (cols, rows) <- I.insert rel dat
  case parseRows fromRow backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

create :: ( MonadStore backend m, Storable backend k l a, ToRow backend l a
          , FromRow backend k (EntityID a) ) => a -> m (EntityID a)
create = fmap head . createMany . pure

updateMany :: forall backend m k l a.
              (MonadStore backend m, Storable backend k l a)
           => UpdateSetter backend a -> Condition backend a -> m Integer
updateMany setter cond = do
  backend <- getBackend
  let rel = relation backend :: Relation backend k l a
  I.update rel setter cond

update :: ( MonadStore backend m, Storable backend k l a
          , ToRow backend k (EntityID a) )
       => EntityID a -> UpdateSetter backend a -> m ()
update i setter = void $ updateMany setter $ EntityID ==. i

deleteMany :: forall backend m k l a.
              (MonadStore backend m, Storable backend k l a)
           => Condition backend a -> m Integer
deleteMany cond = do
  backend <- getBackend
  let rel = relation backend :: Relation backend k l a
  I.delete rel cond

delete :: ( MonadStore backend m, Storable backend k l a
          , ToRow backend k (EntityID a) ) => EntityID a -> m ()
delete i = void $ deleteMany $ EntityID ==. i

class Property backend a f | f -> a where
  toColumns :: backend -> f backend n b -> Vector n Column

(==.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(==.) prop vals =
  buildCondition "=" (flip toColumns prop) (flip toRow vals)

(/=.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(/=.) prop vals =
  buildCondition "<>" (flip toColumns prop) (flip toRow vals)

(<=.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(<=.) prop vals =
  buildCondition "<=" (flip toColumns prop) (flip toRow vals)

(<.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
     -> Condition backend a
(<.) prop vals =
  buildCondition "<" (flip toColumns prop) (flip toRow vals)

(>=.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
      -> Condition backend a
(>=.) prop vals =
  buildCondition ">=" (flip toColumns prop) (flip toRow vals)

(>.) :: (Property backend a f, ToRow backend n b) => f backend n b -> b
     -> Condition backend a
(>.) cols vals =
  buildCondition ">" (flip toColumns cols) (flip toRow vals)

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
              valueList = mconcat $ intersperse ", " $ vectorToList $
                            toRow backend value
          in (Hole (Plain suffix req), Cons ("(" <> valueList <> ")") dat)

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
inList = listHelper "IN"

notInList :: (Property backend a f, ToRow backend n b) => f backend n b -> [b]
          -> Condition backend a
notInList = listHelper "NOT IN"

groupBy :: Property backend a f => f backend n b -> SelectClauses backend a
groupBy prop = mempty { selectGroupBy = vectorToList . flip toColumns prop }

asc :: Property backend a f => f backend n b -> SelectClauses backend a
asc prop =
  let f backend = (False, vectorToList (toColumns backend prop))
  in mempty { selectOrderBy = f }

desc :: Property backend a f => f backend n b -> SelectClauses backend a
desc prop =
  let f backend = (True, vectorToList (toColumns backend prop))
  in mempty { selectOrderBy = f }

limit :: Int -> SelectClauses backend a
limit n = mempty { selectLimit = Just n }

offset :: Int -> SelectClauses backend a
offset n = mempty { selectOffset = Just n }

(=.) :: (Property backend a f, ToRow backend n b)
     => f backend n b -> b -> UpdateSetter backend a
(=.) col value = UpdateSetter $ \backend ->
  vzip (toColumns backend col) (toRow backend value)
