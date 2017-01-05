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
import Database.Seakale.PostgreSQL.FromRow ()
import Database.Seakale.PostgreSQL.ToRow ()
import Database.Seakale.Request.Tests
import Database.Seakale.Store
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

  relation _ = Relation
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
  toColumns _ = \case
    UserEmail    -> ["email"]
    UserPassword -> ["password"]

userIDCols :: [ColumnInfo PSQL]
userIDCols = [ColumnInfo (Just "id") "int4"]

userCols :: [ColumnInfo PSQL]
userCols =
  [ ColumnInfo (Just "email")    "varchar"
  , ColumnInfo (Just "password") "varchar"
  ]

userEntCols :: [ColumnInfo PSQL]
userEntCols = userIDCols ++ userCols

user42IDRow :: Row PSQL
user42IDRow = [Field (Just "42")]

user42Row :: Row PSQL
user42Row =
  [ Field (Just "user42@host")
  , Field (Just "secret")
  ]

user42EntRow :: Row PSQL
user42EntRow = user42IDRow ++ user42Row

user99IDRow :: Row PSQL
user99IDRow = [Field (Just "99")]

user99Row :: Row PSQL
user99Row =
  [ Field (Just "user99@host")
  , Field (Just "secret")
  ]

user99EntRow :: Row PSQL
user99EntRow = user99IDRow ++ user99Row

user42, user99 :: User
user42 = User "user42@host" "secret"
user99 = User "user99@host" "secret"

user42Ent, user99Ent :: Entity User
user42Ent = Entity (UserID 42) user42
user99Ent = Entity (UserID 99) user99

data Post = Post
  { postTitle    :: String
  , postContents :: String
  } deriving (Show, Eq, Generic)

instance Storable PSQL One Two Post where
  data EntityID Post = PostID Integer deriving (Show, Eq, Generic)

  relation _ = Relation
    { relationName      = "posts"
    , relationIDColumns = ["id"]
    , relationColumns   = ["title", "contents"]
    }

instance FromRow PSQL One (EntityID Post)
instance ToRow   PSQL One (EntityID Post)

instance FromRow PSQL Two Post
instance ToRow   PSQL Two Post

postIDCols :: [ColumnInfo PSQL]
postIDCols = [ColumnInfo (Just "id") "int4"]

postCols :: [ColumnInfo PSQL]
postCols =
  [ ColumnInfo (Just "title")    "varchar"
  , ColumnInfo (Just "contents") "varchar"
  ]

postEntCols :: [ColumnInfo PSQL]
postEntCols = postIDCols ++ postCols

post1IDRow :: Row PSQL
post1IDRow = [Field (Just "1")]

post1Row :: Row PSQL
post1Row =
  [ Field (Just "Seakale 101")
  , Field (Just "Seakale is a Haskell library")
  ]

post1EntRow :: Row PSQL
post1EntRow = post1IDRow ++ post1Row

post1 :: Post
post1 = Post "Seakale 101" "Seakale is a Haskell library"

post1Ent :: Entity Post
post1Ent = Entity (PostID 1) post1

data Comment = Comment
  { bpPostID   :: EntityID Post
  , bpUserID   :: Maybe (EntityID User)
  , bpTitle    :: String
  , bpContents :: String
  } deriving (Show, Eq, Generic)

instance Storable PSQL One Four Comment where
  data EntityID Comment = CommentID Integer deriving (Show, Eq, Generic)

  relation _ = Relation
    { relationName      = "comments"
    , relationIDColumns = ["id"]
    , relationColumns   = ["post_id", "user_id", "title", "contents"]
    }

instance FromRow PSQL One (EntityID Comment)
instance ToRow   PSQL One (EntityID Comment)

instance FromRow PSQL Four Comment
instance ToRow   PSQL Four Comment

data CommentProperty b n a where
  CommentPostID :: CommentProperty b One (EntityID Post)
  CommentUserID :: CommentProperty b One (EntityID User)
  CommentTitle  :: CommentProperty b One String

instance Property PSQL Comment CommentProperty where
  toColumns _ = \case
    CommentPostID -> ["post_id"]
    CommentUserID -> ["user_id"]
    CommentTitle  -> ["title"]

commentIDCols :: [ColumnInfo PSQL]
commentIDCols = [ColumnInfo (Just "id") "int4"]

commentCols :: [ColumnInfo PSQL]
commentCols =
  [ ColumnInfo (Just "post_id")  "int4"
  , ColumnInfo (Just "user_id")  "int4"
  , ColumnInfo (Just "title")    "varchar"
  , ColumnInfo (Just "contents") "varchar"
  ]

commentEntCols :: [ColumnInfo PSQL]
commentEntCols = commentIDCols ++ commentCols

comment1337IDRow :: Row PSQL
comment1337IDRow = [Field (Just "1337")]

comment1337Row :: Row PSQL
comment1337Row =
  [ Field (Just "1")
  , Field (Just "42")
  , Field (Just "First comment")
  , Field (Just "First!")
  ]

comment1337EntRow :: Row PSQL
comment1337EntRow = comment1337IDRow ++ comment1337Row

comment1337 :: Comment
comment1337 = Comment (PostID 1) (Just (UserID 42)) "First comment" "First!"

comment1337Ent :: Entity Comment
comment1337Ent = Entity (CommentID 1337) comment1337
