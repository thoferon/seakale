{-# LANGUAGE OverloadedLists #-}

module SpecHelpers
  ( module Test.Hspec
  , module Database.Seakale.Request.Tests
  , module SpecHelpers
  ) where

import GHC.Generics

import Test.Hspec hiding (after)

import Database.Seakale.FromRow
import Database.Seakale.PostgreSQL (PSQL(..), Request)
import Database.Seakale.Request.Tests
import Database.Seakale.Storable
import Database.Seakale.ToRow
import Database.Seakale.Types

run' :: Mock PSQL a -> Request b -> (Either SeakaleError b, Mock PSQL a)
run' = runRequest' PSQL

run :: Mock PSQL a -> Request b -> Either SeakaleError b
run = runRequest PSQL

data User = User
  { userEmail    :: String
  , userPassword :: String -- let's imagine we don't care about security
  } deriving (Show, Eq, Generic)

instance Storable PSQL One Two User where
  data EntityID User = UserID Integer deriving (Show, Eq, Generic)

  relation = Relation
    { relationName      = "users"
    , relationIDColumns = ["id"]
    , relationColumns   = ["email", "password"]
    }

instance FromRow PSQL One (EntityID User)
instance ToRow   PSQL One (EntityID User)

instance FromRow PSQL Two User
instance ToRow   PSQL Two User

data UserProperty b n a where
  UserEmail    :: UserProperty b One String
  UserPassword :: UserProperty b One String

instance Property PSQL User UserProperty where
  toColumns = \case
    UserEmail    -> ["email"]
    UserPassword -> ["password"]

idCol :: ColumnInfo PSQL
idCol = ColumnInfo (Just "id") "int4"

userCols :: [ColumnInfo PSQL]
userCols =
  [ idCol
  , ColumnInfo (Just "email")    "varchar"
  , ColumnInfo (Just "password") "varchar"
  ]

user42Row :: Row PSQL
user42Row =
  [ Field (Just "42")
  , Field (Just "user42@host")
  , Field (Just "secret")
  ]

user99Row :: Row PSQL
user99Row =
  [ Field (Just "99")
  , Field (Just "user99@host")
  , Field (Just "secret")
  ]

user42, user99 :: User
user42 = User "user42@host" "secret"
user99 = User "user99@host" "secret"

user42Ent, user99Ent :: Entity User
user42Ent = Entity (UserID 42) user42
user99Ent = Entity (UserID 99) user99
