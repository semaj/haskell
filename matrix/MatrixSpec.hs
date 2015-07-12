module MatrixSpec where

import Test.Hspec
import Matrix

test1 = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
test2 = [[0, 1, 3], [5, 1, 0], [5, 7, 1]]
vector1 = [[0], [1], [2]]

main :: IO ()
main = hspec $ do
  describe "madd" $ do
    it "adds correctly" $
      madd test1 test2 `shouldBe` [[1, 3, 6], [9, 6, 6], [12, 15, 10]]

  describe "isVector" $ do
    it "returns false when not" $
      isVector test1 `shouldBe` False

    it "returns true when is" $
      isVector vector1 `shouldBe` True

  describe "mvMult" $ do
    it "returns correct"  $
      mvMult test1 vector1 `shouldBe` [[0, 2, 6], [0, 5, 12], [0, 8, 18]]

  describe "rowsToColumns" $ do
    it "should work" $
      rowsToColumns test1 `shouldBe` [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

    it "should work with vector" $
      rowsToColumns vector1 `shouldBe` [[0,1,2]]
