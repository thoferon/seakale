module Database.Seakale.Types where

import           GHC.Exts

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Proxy a = Proxy

data SeakaleError
  = RowParseError String
  | BackendError BS.ByteString

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

class NatEq n m
instance NatEq n n

data Query :: Nat -> * where
  Plain      :: BS.ByteString -> Query n -> Query n
  Hole       :: Query n -> Query ('S n)
  EmptyQuery :: Query Zero

instance IsString (Query n) where
  fromString = undefined

data Field backend = Field
  { fieldValue :: Maybe BS.ByteString
  }

type Row backend = [Field backend]

class Backend backend where
  type ColumnType backend :: *
  type MonadBackend backend (m :: * -> *) :: Constraint

  runQuery :: MonadBackend backend m => Proxy backend -> BSL.ByteString
           -> m (Either BS.ByteString ([ColumnInfo backend], [Row backend]))
  runExecute :: MonadBackend backend m => Proxy backend -> BSL.ByteString
             -> m (Either BS.ByteString Integer)

data ColumnInfo backend = ColumnInfo
  { colInfoName :: Maybe BS.ByteString
  , colInfoType :: ColumnType backend
  }

type QueryData n = Vector n BS.ByteString

data Vector :: Nat -> * -> * where
  Cons :: a -> Vector n a -> Vector ('S n) a
  Nil  :: Vector Zero a

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

newtype Only a = Only a
