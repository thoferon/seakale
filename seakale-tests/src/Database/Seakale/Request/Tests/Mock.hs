module Database.Seakale.Request.Tests.Mock where

import           Control.Applicative

import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Types hiding (runQuery, runExecute)

data Mock backend a
  = MockQuery   (BSL.ByteString -> Bool) ([ColumnInfo backend], [Row backend])
  | MockExecute (BSL.ByteString -> Bool) Integer
  | Or (Mock backend a) (Mock backend a)
  | And (Mock backend a) (Mock backend a)
  | After (Mock backend a) (Mock backend a)
  | None (Maybe a)

notMonadError :: a
notMonadError = error "Mock not intended as an actual monad"

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
  = FMockQuery   (BSL.ByteString -> Bool) ([ColumnInfo backend], [Row backend])
  | FMockExecute (BSL.ByteString -> Bool) Integer
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
                  -> Mock backend a
mockMatchingQuery = MockQuery

mockMatchingExecute :: (BSL.ByteString -> Bool) -> Integer -> Mock backend a
mockMatchingExecute = MockExecute

mockQuery :: BSL.ByteString -> ([ColumnInfo backend], [Row backend])
          -> Mock backend a
mockQuery req dat = MockQuery (==req) dat

mockExecute :: BSL.ByteString -> Integer -> Mock backend a
mockExecute req i = MockExecute (==req) i

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
