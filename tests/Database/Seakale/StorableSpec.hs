module Database.Seakale.StorableSpec where

import Database.Seakale.Storable

import SpecHelpers

spec :: Spec
spec = do
  describe "select" $ do
    it "selects all rows if no filters are given" $ do
      let mock = mockQuery "SELECT id, email, password FROM users"
                           (userCols, [user42Row, user99Row])
          (ents, mock') = run' mock $ select mempty mempty

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [ Entity (UserID 42) (User "user42@host" "secret")
                            , Entity (UserID 99) (User "user99@host" "secret")
                            ]

    it "selects rows with a WHERE clause and other clauses if given" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE password = 'secret'\
                           \ ORDER BY email DESC"
                           (userCols, [user99Row, user42Row])
          (ents, mock') = run' mock $
            select (UserPassword ==. "secret") (desc UserEmail)

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [ Entity (UserID 99) (User "user99@host" "secret")
                            , Entity (UserID 42) (User "user42@host" "secret")
                            ]
