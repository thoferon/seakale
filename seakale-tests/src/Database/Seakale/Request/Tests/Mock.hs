module Database.Seakale.Request.Tests.Mock where

import           Control.Applicative

import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Types hiding (runQuery, runExecute)

data QueryPredicate
  = QPredPlain BSL.ByteString
  | QPredFunction (BSL.ByteString -> Bool)

runQueryPredicate :: QueryPredicate -> BSL.ByteString -> Bool
runQueryPredicate p req = case p of
  QPredPlain req' -> req == req'
  QPredFunction f -> f req

instance Show QueryPredicate where
  show = \case
    QPredPlain bsl  -> show bsl
    QPredFunction _ -> "predicate"

instance Eq QueryPredicate where
  (==) qp1 qp2 = case (qp1, qp2) of
    (QPredPlain q1, QPredPlain q2)     -> q1 == q2
    (QPredPlain q, QPredFunction f)    -> f q
    (QPredFunction f, QPredPlain q)    -> f q
    (QPredFunction _, QPredFunction _) -> False

data Mock backend a
  = MockQuery   QueryPredicate ([ColumnInfo backend], [Row backend])
  | MockExecute QueryPredicate Integer
  | Or (Mock backend a) (Mock backend a)
  | And (Mock backend a) (Mock backend a)
  | After (Mock backend a) (Mock backend a)
  | None (Maybe a)

deriving instance (Show a, Show (ColumnInfo backend)) => Show (Mock backend a)
deriving instance (Eq   a, Eq   (ColumnInfo backend)) => Eq   (Mock backend a)

mockConsumed :: Mock backend a -> Bool
mockConsumed = \case
  None _ -> True
  _ -> False

notMonadError :: a
notMonadError = error "Mock not intended as an actual monad"

instance Monoid (Mock backend a) where
  mempty = None Nothing
  mappend = And

instance Functor (Mock backend) where
  fmap f = \case
    MockQuery req dat -> MockQuery req dat
    MockExecute req i -> MockExecute req i
    Or    m1 m2 -> Or    (fmap f m1) (fmap f m2)
    And   m1 m2 -> And   (fmap f m1) (fmap f m2)
    After m1 m2 -> After (fmap f m1) (fmap f m2)
    None mx -> None (fmap f mx)

instance Applicative (Mock backend) where
  pure = None . Just
  None f <*> fx = fromMaybe notMonadError f <$> fx
  ff <*> None x = ($ fromMaybe notMonadError x) <$> ff
  ff <*> fx = And (castMock ff) (castMock fx)

instance Monad (Mock backend) where
  None mx >>= f = f $ fromMaybe notMonadError mx
  mx >>= f = And (castMock mx) (f notMonadError)

instance Alternative (Mock backend) where
  empty = None Nothing
  mx <|> my = Or mx my

data MockF backend f
  = FMockQuery   QueryPredicate ([ColumnInfo backend], [Row backend])
  | FMockExecute QueryPredicate Integer
  | FOr f f
  | FAnd f f
  | FAfter f f
  | FNone
  deriving Functor

type instance Base (Mock backend a) = MockF backend

instance Recursive (Mock backend a) where
  project = \case
    MockQuery   i o -> FMockQuery   i o
    MockExecute i o -> FMockExecute i o
    Or    m1 m2 -> FOr    m1 m2
    And   m1 m2 -> FAnd   m1 m2
    After m1 m2 -> FAfter m1 m2
    None _ -> FNone

castMock :: Mock backend a -> Mock backend b
castMock = cata cast
  where
    cast :: MockF backend (Mock backend b) -> Mock backend b
    cast = \case
      FMockQuery i o -> MockQuery i o
      FMockExecute i o -> MockExecute i o
      FOr    m1 m2 -> Or    m1 m2
      FAnd   m1 m2 -> And   m1 m2
      FAfter m1 m2 -> After m1 m2
      FNone -> None Nothing

mockMatchingQuery :: (BSL.ByteString -> Bool)
                  -> ([ColumnInfo backend], [Row backend])
                  -> Mock backend ()
mockMatchingQuery f dat = MockQuery (QPredFunction f) dat

mockMatchingExecute :: (BSL.ByteString -> Bool) -> Integer -> Mock backend ()
mockMatchingExecute f i = MockExecute (QPredFunction f) i

mockQuery :: BSL.ByteString -> ([ColumnInfo backend], [Row backend])
          -> Mock backend ()
mockQuery req dat = MockQuery (QPredPlain req) dat

mockExecute :: BSL.ByteString -> Integer -> Mock backend ()
mockExecute req i = MockExecute (QPredPlain req) i

mor :: Mock backend a -> Mock backend a -> Mock backend a
mor = Or

mand :: Mock backend a -> Mock backend a -> Mock backend a
mand = And

after, andThen :: Mock backend a -> Mock backend a -> Mock backend a
after   = After
andThen = After

anyOf, allOf :: [Mock backend a] -> Mock backend a
anyOf = foldr mor  (None Nothing)
allOf = foldr mand (None Nothing)

fixAfter :: Mock backend a -> Mock backend a
fixAfter mock = After mock (fixAfter mock)

times :: Int -> Mock backend a -> Mock backend a
times n mock
  | n <= 0 = None Nothing
  | n == 1 = mock
  | otherwise = After mock $ times (n - 1) mock
