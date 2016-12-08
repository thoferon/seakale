module Database.Seakale.Storable.Join
  ( Join
  , JoinLeftProperty(..)
  , JoinRightProperty(..)
  , LeftJoin(..)
  , selectJoin
  , selectJoin_
  , table
  , leftJoin
  , leftJoin_
  , JoinCondition
  , (~.)
  ) where

import           Data.List
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS

import           Database.Seakale.FromRow
import           Database.Seakale.Storable
import           Database.Seakale.Types
import qualified Database.Seakale.Storable.Internal as I

data Join a b

data JoinLeftProperty  f b backend (n :: Nat) c = JLeft  (f backend n c)
data JoinRightProperty f a backend (n :: Nat) c = JRight (f backend n c)

instance Property backend a f
  => Property backend (Join a b) (JoinLeftProperty f b) where
  toColumns (JLeft prop) =
    fmap (\col -> Column $ unColumn col . (<> "l")) (toColumns prop)

instance Property backend b f
  => Property backend (Join a b) (JoinRightProperty f a) where
  toColumns (JRight prop) =
    fmap (\col -> Column $ unColumn col . (<> "r")) (toColumns prop)

class IsJoin f where
  castCondition :: Condition backend (Join a b) -> Condition backend (f a b)
  castCondition = I.unsafeCastCondition

  castSelectClauses :: SelectClauses backend (Join a b)
                    -> SelectClauses backend (f a b)
  castSelectClauses = I.unsafeCastSelectClauses

data LeftJoin a b = LeftJoin (Entity a) (Maybe (Entity b))

instance IsJoin LeftJoin

instance ( FromRow backend k (EntityID a), FromRow backend l a
         , FromRow backend i (Maybe (EntityID b)), FromRow backend j (Maybe b)
         , (k :+ i :+ l :+ j) ~ n )
  => FromRow backend n (LeftJoin a b) where
  fromRow = (\li mri lv mrv -> LeftJoin (Entity li lv) (Entity <$> mri <*> mrv))
              `pmap` fromRow `papply` fromRow `papply` fromRow `papply` fromRow

selectJoin :: ( MonadSelect backend m, IsJoin f
              , Storable backend k l a , Storable backend i j b
              , FromRow backend ((k :+ l) :+ (i :+ j)) (f a b) )
           => (backend -> Relation backend (k :+ i) (l :+ j) (f a b))
           -> Condition backend (Join a b) -> SelectClauses backend (Join a b)
           -> m [f a b]
selectJoin rel cond clauses = do
  let cond'    = castCondition cond
      clauses' = castSelectClauses clauses
  backend <- getBackend
  (cols, rows) <- I.select (rel backend) cond' clauses'
  case parseRows fromRow backend cols rows of
    Left err -> throwSeakaleError $ RowParseError err
    Right xs -> return xs

selectJoin_ :: ( MonadSelect backend m, IsJoin f
               , Storable backend k l a , Storable backend i j b
               , FromRow backend ((k :+ l) :+ (i :+ j)) (f a b) )
            => (backend -> Relation backend (k :+ i) (l :+ j) (f a b))
            -> Condition backend (Join a b) -> m [f a b]
selectJoin_ rel cond = selectJoin rel cond mempty

leftJoin :: (backend -> Relation backend k l a)
         -> (backend -> Relation backend i j b) -> JoinCondition backend a b
         -> backend -> Relation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin = mkJoin "LEFT JOIN"

leftJoin_ :: (Storable backend k l a, Storable backend i j b)
          => JoinCondition backend a b
          -> backend -> Relation backend (k :+ i) (l :+ j) (LeftJoin a b)
leftJoin_ = leftJoin table table

-- FIXME: It should use Condition backend a b instead.
newtype JoinCondition backend a b
  = JoinCondition (backend -> [(Column, Column)])
  deriving Monoid

-- FIXME: How to make sure c and d are compatible types? It should still be
-- possible to use (leftJoin (EntityID ~. EntityID)) so we can use c twice.
(~.) :: (Property backend a f, Property backend b g)
     => f backend n c -> g backend n d -> JoinCondition backend a b
(~.) prop1 prop2 =
  let cols1 = vectorToList $ toColumns prop1
      cols2 = vectorToList $ toColumns prop2
  in JoinCondition $ \_ -> zip cols1 cols2

table :: Storable backend k l a => backend -> Relation backend k l a
table _ = relation

mkJoin :: BS.ByteString -> (backend -> Relation backend k l a)
       -> (backend -> Relation backend i j b)
       -> JoinCondition backend a b
       -> backend -> Relation backend (k :+ i) (l :+ j) (LeftJoin a b)
mkJoin joinStmt relA relB (JoinCondition f) backend =
  combineRelations joinStmt (relA backend) (relB backend) (f backend)

buildJoinConditions :: BS.ByteString -> [(Column, Column)] -> BS.ByteString
buildJoinConditions prefix =
  let buildCond (colA, colB) =
        unColumn colA (prefix <> "l") <> " = " <> unColumn colB (prefix <> "r")
  in mconcat . intersperse " AND " . map buildCond

combineRelationNames :: BS.ByteString -> RelationName -> RelationName
                     -> [(Column, Column)] -> RelationName
combineRelationNames joinStmt l r pairs = RelationName $ \prefix ->
  unRelationName l (prefix <> "l")
  <> " " <> joinStmt <> " "
  <> unRelationName r (prefix <> "r")
  <> if null pairs then "" else " ON " <> buildJoinConditions prefix pairs

combineRelationColumns :: Vector k Column -> Vector l Column
                       -> Vector (k :+ l) Column
combineRelationColumns l r =
  fmap (\col -> Column $ unColumn col . (<> "l")) l
  `vappend`
  fmap (\col -> Column $ unColumn col . (<> "r")) r

combineRelations :: BS.ByteString -> Relation backend k l a
                 -> Relation backend i j b -> [(Column, Column)]
                 -> Relation backend (k :+ i) (l :+ j) c
combineRelations joinStmt relA relB pairs = Relation
  { relationName =
      combineRelationNames joinStmt (relationName relA) (relationName relB)
                           pairs
  , relationIDColumns =
      combineRelationColumns (relationIDColumns relA) (relationIDColumns relB)
  , relationColumns =
      combineRelationColumns (relationColumns relA) (relationColumns relB)
  }
