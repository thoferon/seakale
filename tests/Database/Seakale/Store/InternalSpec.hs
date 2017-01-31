module Database.Seakale.Store.InternalSpec where

import Data.Monoid

import Database.Seakale.PostgreSQL (PSQL(..))
import Database.Seakale.Store (desc, asc)
import Database.Seakale.Store.Internal

import SpecHelpers

spec :: Spec
spec = do
  describe "SelectClauses" $ do
    describe "instance Monoid" $ do
      it "overwrites the ORDER BY clauses from the left clauses if\
         \ duplicate" $ do
        let clauses1 = desc CommentPostID <> desc CommentUserID
            clauses2 = asc  CommentTitle  <> asc  CommentUserID
        selectOrderBy (clauses1 <> clauses2) PSQL
          `shouldBe` [("post_id", Desc), ("title", Asc), ("user_id", Asc)]
