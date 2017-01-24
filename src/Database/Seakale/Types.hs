module Database.Seakale.Types where

import           GHC.Exts

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data SeakaleError
  = RowParseError String
  | BackendError BS.ByteString
  | EntityNotFoundError
  deriving (Show, Eq)

class Monad m => MonadSeakaleBase backend m | m -> backend where
  getBackend        :: m backend
  throwSeakaleError :: SeakaleError -> m a
  catchSeakaleError :: m a -> (SeakaleError -> m a) -> m a

instance MonadSeakaleBase backend m
  => MonadSeakaleBase backend (ExceptT e m) where
  getBackend        = lift getBackend
  throwSeakaleError = lift . throwSeakaleError
  catchSeakaleError f handler = ExceptT $
    (catchSeakaleError (runExceptT f) (runExceptT . handler))

instance MonadSeakaleBase backend m
  => MonadSeakaleBase backend (ReaderT r m) where
  getBackend        = lift getBackend
  throwSeakaleError = lift . throwSeakaleError
  catchSeakaleError f handler = do
    r <- ask
    lift $ catchSeakaleError (runReaderT f r) (flip runReaderT r . handler)

instance MonadSeakaleBase backend m
  => MonadSeakaleBase backend (StateT s m) where
  getBackend        = lift getBackend
  throwSeakaleError = lift . throwSeakaleError
  catchSeakaleError f handler = do
    s <- get
    (x, s') <- lift $
      catchSeakaleError (runStateT f s) (flip runStateT s . handler)
    put s'
    return x

instance (Monoid w, MonadSeakaleBase backend m)
  => MonadSeakaleBase backend (WriterT w m) where
  getBackend        = lift getBackend
  throwSeakaleError = lift . throwSeakaleError
  catchSeakaleError f handler =
    lift (catchSeakaleError (runWriterT f) (runWriterT . handler)) >>= writer

data Nat = O | S Nat

type Zero  = 'O
type One   = 'S Zero
type Two   = 'S One
type Three = 'S Two
type Four  = 'S Three
type Five  = 'S Four
type Six   = 'S Five
type Seven = 'S Six
type Eight = 'S Seven
type Nine  = 'S Eight
type Ten   = 'S Nine

type family (:+) (n :: Nat) (m :: Nat) :: Nat
type instance 'O :+ n = n
type instance 'S n :+ m = 'S (n :+ m)

data Query :: Nat -> * where
  Plain      :: BS.ByteString -> Query n -> Query n
  Hole       :: Query n -> Query ('S n)
  EmptyQuery :: Query Zero

eqQuery :: Query n -> Query m -> Bool
eqQuery = curry $ \case
  (Plain bs q, Plain bs' q') -> bs == bs' && q `eqQuery` q'
  (Hole q, Hole q') -> q `eqQuery` q'
  (EmptyQuery, EmptyQuery) -> True
  _ -> False

qappend :: Query n -> Query m -> Query (n :+ m)
qappend q1 q2 = case q1 of
  Plain bs q1' -> Plain bs (qappend q1' q2)
  Hole     q1' -> Hole     (qappend q1' q2)
  EmptyQuery   -> q2

-- Hack to prevent GHC to fail on (n :+ 'O) ~ n with qappend
qappendZero :: Query n -> Query Zero -> Query n
qappendZero q1 q2 = case q1 of
  Plain bs q1' -> Plain bs (qappendZero q1' q2)
  Hole     q1' -> Hole     (qappendZero q1' q2)
  EmptyQuery   -> q2

parenthesiseQuery :: Query n -> Query n
parenthesiseQuery q = Plain "(" $ q `qappendZero` Plain ")" EmptyQuery

data RepeatQuery :: Nat -> Nat -> Nat -> * where
  RepeatQuery :: Query k -> Query l -> BSL.ByteString -> Query i
              -> RepeatQuery k l i

formatQuery :: Query n -> QueryData n -> BSL.ByteString
formatQuery r d = BSL.fromChunks $ go r d
  where
    go :: Query n -> QueryData n -> [BS.ByteString]
    go req dat = case (req, dat) of
      (Plain bs req', _) -> bs : go req' dat
      (Hole req', Cons bs dat') -> bs : go req' dat'
      (EmptyQuery, Nil) -> []

formatMany :: RepeatQuery k l i -> QueryData k -> QueryData i -> [QueryData l]
           -> BSL.ByteString
formatMany (RepeatQuery before between sep after) beforeData afterData dat =
  formatQuery before beforeData
  <> mconcat (intersperse sep (map (formatQuery between) dat))
  <> formatQuery after afterData

newtype Field backend
  = Field { fieldValue :: Maybe BS.ByteString }
  deriving (Show, Eq)

type Row backend = [Field backend]

class Backend backend where
  type ColumnType backend :: *
  type MonadBackend backend (m :: * -> *) :: Constraint

  runQuery :: MonadBackend backend m => backend -> BSL.ByteString
           -> m (Either BS.ByteString ([ColumnInfo backend], [Row backend]))
  runExecute :: MonadBackend backend m => backend -> BSL.ByteString
             -> m (Either BS.ByteString Integer)

data ColumnInfo backend = ColumnInfo
  { colInfoName :: Maybe BS.ByteString
  , colInfoType :: ColumnType backend
  }

deriving instance Show (ColumnType backend) => Show (ColumnInfo backend)
deriving instance Eq   (ColumnType backend) => Eq   (ColumnInfo backend)

type QueryData n = Vector n BS.ByteString

data Vector :: Nat -> * -> * where
  Cons :: a -> Vector n a -> Vector ('S n) a
  Nil  :: Vector Zero a

deriving instance Eq a => Eq (Vector n a)

instance Functor (Vector n) where
  fmap f = \case
    Cons x xs -> Cons (f x) (fmap f xs)
    Nil       -> Nil

cons, (<:>) :: a -> Vector n a -> Vector ('S n) a
cons = Cons
(<:>) = cons

infixr 5 <:>

nil :: Vector Zero a
nil = Nil

(<:|) :: a -> a -> Vector Two a
(<:|) x y = x <:> y <:> nil

infixr 5 <:|

vappend :: Vector n a -> Vector m a -> Vector (n :+ m) a
vappend Nil xs = xs
vappend (Cons x xs) ys = Cons x (vappend xs ys)

vzip :: Vector n a -> Vector n b -> Vector n (a, b)
vzip = curry $ \case
  (Cons x xs, Cons y ys) -> Cons (x, y) (vzip xs ys)
  (Nil, Nil) -> Nil

vectorToList :: Vector n a -> [a]
vectorToList = \case
  Nil -> []
  Cons x xs -> x : vectorToList xs

singleton :: a -> Vector One a
singleton x = Cons x Nil

eqVector :: Eq a => Vector n a -> Vector m a -> Bool
eqVector = curry $ \case
  (Nil, Nil) -> True
  (Cons x xs, Cons y ys) -> x == y && eqVector xs ys
  _ -> False

instance IsList (Vector Zero a) where
  type Item (Vector Zero a) = a

  fromList [] = Nil
  fromList _ = error "IsList (Vector n): too many elements"

  toList Nil = []

instance (IsList (Vector n a), Item (Vector n a) ~ a)
  => IsList (Vector ('S n) a) where
  type Item (Vector ('S n) a) = a

  fromList [] = error "IsList (Vector n): too few elements"
  fromList (x:xs) = Cons x (fromList xs)

  toList (Cons x xs) = x : toList xs

class NTimes f where
  ntimes :: a -> f a

instance NTimes (Vector Zero) where
  ntimes _ = Nil

instance NTimes (Vector n) => NTimes (Vector ('S n)) where
  ntimes x = Cons x (ntimes x)

data Null = Null
