module Database.Seakale.Request.Tests.Mock where

import           Data.Functor.Foldable
import qualified Data.ByteString.Lazy as BSL

import           Database.Seakale.Types hiding (runQuery, runExecute)

data Mock backend
  = MockQuery BSL.ByteString ([ColumnInfo backend], [Row backend])
  | MockExecute BSL.ByteString Integer
  | Or (Mock backend) (Mock backend)
  | And (Mock backend) (Mock backend)
  | After (Mock backend) (Mock backend)
  | None

data MockF backend f
  = FMockQuery BSL.ByteString ([ColumnInfo backend], [Row backend])
  | FMockExecute BSL.ByteString Integer
  | FOr f f
  | FAnd f f
  | FAfter f f
  | FNone
  deriving Functor

type instance Base (Mock backend) = MockF backend

instance Recursive (Mock backend) where
  project = \case
    MockQuery   i o -> FMockQuery   i o
    MockExecute i o -> FMockExecute i o
    Or    m1 m2 -> FOr    m1 m2
    And   m1 m2 -> FAnd   m1 m2
    After m1 m2 -> FAfter m1 m2
    None -> FNone

mockQuery :: BSL.ByteString -> ([ColumnInfo backend], [Row backend])
          -> Mock backend
mockQuery = MockQuery

mockExecute :: BSL.ByteString -> Integer -> Mock backend
mockExecute = MockExecute

mor :: Mock backend -> Mock backend -> Mock backend
mor = Or

mand :: Mock backend -> Mock backend -> Mock backend
mand = And

after, andThen :: Mock backend -> Mock backend -> Mock backend
after   = After
andThen = After

anyOf, allOf :: [Mock backend] -> Mock backend
anyOf = foldr mor  None
allOf = foldr mand None
