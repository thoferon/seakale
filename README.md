Seakale is a set of libraries to write pure code interacting with a SQL
database. The goal is for this code to be testable but running it a different
interpreter mocking the database calls. The base package `seakale` provides the
generic function and is backend agnostic. For now, there is only
`seakale-postgresql` to actually connect to the database and run the requests.
For tests, you can use `seakale-tests` which allows you to create mocks and
provide you with an alternative way of running the code.

# Overview

Seakale exports three monads: `Request`, `Select` and `Store` (also `RequestT`,
`SelectT` and `StoreT`) together with their corresponding type classes:
`MonadRequest`, `MonadSelect` and `MonadStore`.

## FromRow/ToRow

These two type classes are used to parse and write values of their type from/to
the database. Whether you are using `Request` or the other two, you should
define instances for your types.

Note that they track the number of columns consumed by the value at the
type-level.

Also, you can use generics to avoid having to write the instances.

## Request

This is the simplest one in which you can send custom SQL request to the
database. The module exports functions such as `query` and `returning`. This is
a lower-level interface to the database.

## Select and Store

`Select` allows you to send SELECT request as its name indicates while `Store`
is an extension of `Select` which adds `INSERT`, `UPDATE` and `DELETE` requests.
This is so you can mark some of your code as read-only like the following.
Notice how `someOtherAction` can call `someReadOnlyAction`.

```haskell
someReadOnlyAction :: MonadSelect backend m => m MyResult

someOtherAction :: MonadStore backend m => m ()
someOtherAction = do
  res <- someReadOnlyAction
  -- ...
  return ()
```

The functions for those monads are designed to work with types instantiating
`Storable`. These types have an associated type for their identifier and a
corresponding relation. See this example.

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

data Person = Person
  { personFirstName :: String
  , personLastName  :: String
  } deriving Generic

instance FromRow PSQL Two Person
instance ToRow   PSQL Two Person

instance Storable PSQL One Two Person where
  data EntityID Person = PersonID { unPersonID :: Integer } deriving Generic
  relation _ = Relation
    { relationName      = "people"
    , relationIDColumns = ["id"]
    , relationColumns   = ["first_name", "last_name"]
    }

instance FromRow PSQL One (EntityID Person)
instance ToRow   PSQL One (EntityID Person)
```

# Joins

In addition to simple SELECT requests, you can select on (arbitrarily nested)
joins. Supported joins are `LEFT JOIN`, `RIGHT JOIN`, `INNER JOIN` and
`FULL JOIN`. Note you should be in a monad instantiating `MonadSelect`.

# Examples

There are a few examples in the directory `demo/` of the repository.

# Running the code

Packages such as `seakale-postgresql` export monads and functions such as
`Request` and `runRequest`.

The package `seakale-tests` exports such functions which take a mock of the
database so the tests run in a pure environment.
