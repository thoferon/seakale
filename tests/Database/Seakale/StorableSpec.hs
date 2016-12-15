module Database.Seakale.StorableSpec where

import Database.Seakale.Storable
import Database.Seakale.Types

import SpecHelpers

spec :: Spec
spec = do
  describe "select" $ do
    it "selects all rows if no filters are given" $ do
      let mock = mockQuery "SELECT id, email, password FROM users"
                           (userEntCols, [user42EntRow, user99EntRow])
          (ents, mock') = run' mock $ select mempty mempty

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [user42Ent, user99Ent]

    it "selects rows with a WHERE clause and other clauses if given" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE password = 'secret'\
                           \ ORDER BY email DESC"
                           (userEntCols, [user99EntRow, user42EntRow])
          (ents, mock') = run' mock $
            select (UserPassword ==. "secret") (desc UserEmail)

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [user99Ent, user42Ent]

  describe "getMany" $ do
    it "selects all entities with the given IDs" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE (id) IN ((42), (99))"
                           (userEntCols, [user42EntRow, user99EntRow])
          (ents, mock') = run' mock $ getMany [UserID 42, UserID 99]

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [user42Ent, user99Ent]

  describe "getMaybe" $ do
    it "selects the value with the given ID" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 42 LIMIT 1"
                           (userEntCols, [user42EntRow])
          (ents, mock') = run' mock $ getMaybe $ UserID 42

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right (Just user42)

    it "returns Nothing if it does not exist" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 43 LIMIT 1" (userCols, [])
          (ents, mock') = run' mock $ getMaybe $ UserID 43

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right Nothing

  describe "get" $ do
    it "selects the value with the given ID" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 42 LIMIT 1"
                           (userEntCols, [user42EntRow])
          (ents, mock') = run' mock $ get $ UserID 42

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right user42

    it "throws EntityNotFoundError if it does not exist" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 43 LIMIT 1" (userEntCols, [])
          (ents, mock') = run' mock $ get $ UserID 43

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Left EntityNotFoundError

  describe "createMany" $ do
    it "inserts new values" $ do
      let mock = mockQuery "INSERT INTO users (email, password) VALUES\
                           \ ('user42@host', 'secret')\
                           \ ('user99@host', 'secret') RETURNING id"
                           (userIDCols, [user42IDRow, user99IDRow])
          (ents, mock') = run' mock $ createMany [user42, user99]

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [UserID 42, UserID 99]

  describe "updateMany" $ do
    it "updates rows matching some condition" $ do
      let mock = mockExecute "UPDATE users SET password = 'secret'\
                             \ WHERE email = 'user42@host'" 1
          (ents, mock') = run' mock $
            updateMany (UserPassword =. "secret") (UserEmail ==. "user42@host")

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right 1

  describe "update" $ do
    it "updates a row given its ID" $ do
      let mock = mockExecute "UPDATE users SET password = 'secret'\
                             \ WHERE id = 42" 1
          (ents, mock') = run' mock $
            update (UserID 42) (UserPassword =. "secret")

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right ()

  describe "deleteMany" $ do
    it "deletes rows matching some condition" $ do
      let mock = mockExecute "DELETE FROM users WHERE password = 'secret'" 2
          (ents, mock') = run' mock $ deleteMany $ UserPassword ==. "secret"

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right 2

  describe "delete" $ do
    it "deletes a row given its ID" $ do
      let mock = mockExecute "DELETE FROM users WHERE id = 42" 1
          (ents, mock') = run' mock $ delete $ UserID 42

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right ()
