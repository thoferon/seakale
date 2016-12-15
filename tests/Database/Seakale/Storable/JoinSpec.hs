module Database.Seakale.Storable.JoinSpec where

import Database.Seakale.Storable
import Database.Seakale.Storable.Join

import SpecHelpers

spec :: Spec
spec = do
  describe "selectJoin" $ do
    it "selects from several relations" $ do
      let req = "SELECT ll.id, lr.id, r.id, ll.post_id, ll.user_id, ll.title,\
                \ ll.contents, lr.title, lr.contents, r.email, r.password FROM\
                \ comments AS ll INNER JOIN posts AS lr ON ll.post_id = lr.id\
                \ LEFT JOIN users AS r ON ll.user_id = r.id\
                \ WHERE ll.title <> '' ORDER BY r.email ASC"
          mock = mockQuery req
                           ( commentIDCols ++ postIDCols ++ userIDCols
                             ++ commentCols ++ postCols ++ userCols
                           , [ comment1337IDRow ++ post1IDRow ++ user42IDRow
                                ++ comment1337Row ++ post1Row ++ user42Row
                             ]
                           )
          (ents, mock') = run' mock $ do
            let rel = (leftJoin
                        (innerJoin_ (JLeft CommentPostID ==# JRight EntityID))
                        table
                        (JLeft (JLeft CommentUserID) ==# JRight EntityID))
            selectJoin rel
                       (JLeft (JLeft CommentTitle) /=. "")
                       (asc (JRight UserEmail))

      mock' `shouldSatisfy` mockConsumed
      ents `shouldBe` Right [Entity (LeftJoinID (InnerJoinID (CommentID 1337)
                                                             (PostID 1))
                                                (Just (UserID 42)))
                                    (LeftJoin (InnerJoin comment1337 post1)
                                              (Just user42))]