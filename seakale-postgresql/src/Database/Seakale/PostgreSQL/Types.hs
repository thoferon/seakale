module Database.Seakale.PostgreSQL.Types
  ( PSQL(..)
  , SeakaleError(..)
  , T.Query(..)
  , RequestT
  , Request
  , T.Field(..)
  , T.Row
  , T.ColumnInfo(..)
  , T.QueryData
  , T.Vector(..)
  , T.cons, (<:>)
  , T.nil, (<:|)
  , T.Zero
  , T.One
  , T.Two
  , T.Three
  , T.Four
  , T.Five
  , T.Six
  , T.Seven
  , T.Eight
  , T.Nine
  , T.Ten
  ) where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans.Free

import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL

import           Database.PostgreSQL.LibPQ hiding (status, Row)

import           Database.Seakale.Types
                   ( SeakaleError(..), Backend(MonadBackend), ColumnInfo(..)
                   , Row, Field(..), (<:>), (<:|) )
import qualified Database.Seakale.Types as T
import qualified Database.Seakale.Request.Internal as I

data PSQL = PSQL

type RequestT = I.RequestT PSQL
type Request  = I.Request  PSQL
