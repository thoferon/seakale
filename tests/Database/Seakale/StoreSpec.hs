module Database.Seakale.StoreSpec where

import Database.Seakale.Store
import Database.Seakale.Types

import SpecHelpers

spec :: Spec
spec = do
  describe "select" $ do
    it "selects all rows if no filters are given" $ do
      let mock = mockQuery "SELECT id, email, password FROM users"
                           (userEntCols, [user42EntRow, user99EntRow])

      run mock (select mempty mempty)
        `shouldReturn` Right [user42Ent, user99Ent]

    it "selects rows with a WHERE clause and other clauses if given" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE password = 'secret'\
                           \ ORDER BY email DESC"
                           (userEntCols, [user99EntRow, user42EntRow])

      run mock (select (UserPassword ==. "secret") (desc UserEmail))
        `shouldReturn` Right [user99Ent, user42Ent]

  describe "getMany" $ do
    it "selects all entities with the given IDs" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE (id) IN ((42), (99))"
                           (userEntCols, [user42EntRow, user99EntRow])

      run mock (getMany [UserID 42, UserID 99])
        `shouldReturn` Right [user42Ent, user99Ent]

  describe "getMaybe" $ do
    it "selects the value with the given ID" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 42 LIMIT 1"
                           (userEntCols, [user42EntRow])

      run mock (getMaybe (UserID 42)) `shouldReturn` Right (Just user42)

    it "returns Nothing if it does not exist" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 43 LIMIT 1" (userCols, [])

      run mock (getMaybe (UserID 43)) `shouldReturn` Right Nothing

  describe "get" $ do
    it "selects the value with the given ID" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 42 LIMIT 1"
                           (userEntCols, [user42EntRow])

      run mock (get (UserID 42)) `shouldReturn` Right user42

    it "throws EntityNotFoundError if it does not exist" $ do
      let mock = mockQuery "SELECT id, email, password FROM users\
                           \ WHERE id = 43 LIMIT 1" (userEntCols, [])

      run mock (get (UserID 43)) `shouldReturn` Left EntityNotFoundError

  describe "insertMany" $ do
    it "inserts new values" $ do
      let mock = mockQuery "INSERT INTO users (email, password) VALUES\
                           \ ('user42@host', 'secret'),\
                           \ ('user99@host', 'secret') RETURNING id"
                           (userIDCols, [user42IDRow, user99IDRow])

      run mock (insertMany [user42, user99])
        `shouldReturn` Right [UserID 42, UserID 99]

  describe "updateMany" $ do
    it "updates rows matching some condition" $ do
      let mock = mockExecute "UPDATE users SET password = 'secret'\
                             \ WHERE email = 'user42@host'" 1

      run mock (updateMany (UserPassword =. "secret")
                           (UserEmail ==. "user42@host"))
        `shouldReturn` Right 1

  describe "update" $ do
    let req = "UPDATE users SET password = 'secret' WHERE id = 42"

    it "updates a row given its ID" $ do
      let mock = mockExecute req 1
      run mock (update (UserID 42) (UserPassword =. "secret"))
        `shouldReturn` Right ()

    it "throws EntityNotFoundError if no row was modified" $ do
      let mock = mockExecute req 0
      run mock (update (UserID 42) (UserPassword =. "secret"))
        `shouldReturn` Left EntityNotFoundError

  describe "save" $ do
    let user = User "fake@server.tld" "secret"
        req  = "UPDATE users SET email = 'fake@server.tld', password = 'secret'\
               \ WHERE id = 42"

    it "updates a row given its ID" $ do
      let mock = mockExecute req 1
      run mock (save (UserID 42) user) `shouldReturn` Right ()

    it "throws EntityNotFoundError if no row was modified" $ do
      let mock = mockExecute req 0
      run mock (save (UserID 42) user) `shouldReturn` Left EntityNotFoundError

  describe "deleteMany" $ do
    it "deletes rows matching some condition" $ do
      let mock = mockExecute "DELETE FROM users WHERE password = 'secret'" 2
      run mock (deleteMany (UserPassword ==. "secret")) `shouldReturn` Right 2

  describe "delete" $ do
    it "deletes a row given its ID" $ do
      let mock = mockExecute "DELETE FROM users WHERE id = 42" 1
      run mock (delete (UserID 42)) `shouldReturn` Right ()

    it "throws EntityNotFoundError if no row was deleted" $ do
      let mock = mockExecute "DELETE FROM users WHERE id = 42" 0
      run mock (delete (UserID 42)) `shouldReturn` Left EntityNotFoundError
