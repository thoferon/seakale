module Database.Seakale.Tests.Mock where

import           Control.Applicative

import           Data.Functor.Foldable
import           Data.Maybe
import qualified Data.ByteString.Lazy as BSL

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

data Mock sub a
  = Action sub
  | Or (Mock sub a) (Mock sub a)
  | And (Mock sub a) (Mock sub a)
  | After (Mock sub a) (Mock sub a)
  | None (Maybe a)
  deriving (Show, Eq)

mockConsumed :: Mock sub a -> Bool
mockConsumed = \case
  None _ -> True
  _ -> False

notMonadError :: a
notMonadError = error "Mock not intended as an actual monad"

instance Monoid (Mock sub a) where
  mempty = None Nothing
  mappend = And

instance Functor (Mock sub) where
  fmap f = \case
    Action sub  -> Action sub
    Or    m1 m2 -> Or    (fmap f m1) (fmap f m2)
    And   m1 m2 -> And   (fmap f m1) (fmap f m2)
    After m1 m2 -> After (fmap f m1) (fmap f m2)
    None mx -> None (fmap f mx)

instance Applicative (Mock sub) where
  pure = None . Just
  None f <*> fx = fromMaybe notMonadError f <$> fx
  ff <*> None x = ($ fromMaybe notMonadError x) <$> ff
  ff <*> fx = And (castMock ff) (castMock fx)

instance Monad (Mock sub) where
  None mx >>= f = f $ fromMaybe notMonadError mx
  mx >>= f = After (castMock mx) (f notMonadError)

instance Alternative (Mock sub) where
  empty = None Nothing
  mx <|> my = Or mx my

data MockF sub f
  = FAction sub
  | FOr f f
  | FAnd f f
  | FAfter f f
  | FNone
  deriving Functor

type instance Base (Mock sub a) = MockF sub

instance Recursive (Mock sub a) where
  project = \case
    Action sub  -> FAction sub
    Or    m1 m2 -> FOr    m1 m2
    And   m1 m2 -> FAnd   m1 m2
    After m1 m2 -> FAfter m1 m2
    None _ -> FNone

consumeMock :: (sub -> Maybe b) -> Mock sub a -> Maybe (b, Mock sub a)
consumeMock = para . phi
  where
    phi :: (sub -> Maybe a) -> MockF sub (Mock sub c, Maybe (a, Mock sub c))
        -> Maybe (a, Mock sub c)
    phi f = \case
      FAction sub -> (, None Nothing) <$> f sub
      FOr (_, mRes1) (_, mRes2) -> mRes1 <|> mRes2
      FAnd (m1, mRes1) (m2, mRes2) ->
        case (mRes1, mRes2) of
          (Just (x, m1'), _) -> Just (x, noNone And m1' m2)
          (_, Just (x, m2')) -> Just (x, noNone And m1 m2')
          _ -> Nothing
      FAfter (_, mRes1) (m2, _) ->
        fmap (\(x, m1') -> (x, noNone After m1' m2)) mRes1
      FNone -> Nothing

    noNone :: (Mock sub a -> Mock sub a -> Mock sub a)
           -> Mock sub a -> Mock sub a -> Mock sub a
    noNone _ (None _) m = m
    noNone g m1 m2 = g m1 m2

castMock :: Mock sub a -> Mock sub b
castMock = cata cast
  where
    cast :: MockF sub (Mock sub b) -> Mock sub b
    cast = \case
      FAction sub  -> Action sub
      FOr    m1 m2 -> Or    m1 m2
      FAnd   m1 m2 -> And   m1 m2
      FAfter m1 m2 -> After m1 m2
      FNone -> None Nothing

mor :: Mock sub a -> Mock sub a -> Mock sub a
mor = Or

mand :: Mock sub a -> Mock sub a -> Mock sub a
mand = And

after, andThen :: Mock sub a -> Mock sub a -> Mock sub a
after   = After
andThen = After

anyOf, allOf :: [Mock sub a] -> Mock sub a
anyOf = foldr mor  (None Nothing)
allOf = foldr mand (None Nothing)

fixAfter :: Mock sub a -> Mock sub a
fixAfter mock = After mock (fixAfter mock)

times :: Int -> Mock sub a -> Mock sub a
times n mock
  | n <= 0 = None Nothing
  | n == 1 = mock
  | otherwise = After mock $ times (n - 1) mock
