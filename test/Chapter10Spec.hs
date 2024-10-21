module Chapter10Spec where

import Chapter10
import Test.Hspec

spec :: Spec
spec = do
  describe "Chapter10" $ do
    it "isNumber" $ do
      isNumber' "a" `shouldBe` False
      isNumber' "9" `shouldBe` True
      isNumber' "10" `shouldBe` True
      isNumber' "a0" `shouldBe` False

    it "nextgen game of life" $ do
      nextgen glider `shouldBe` [(4, 3), (3, 4), (4, 4), (3, 2), (5, 3)]

    it "show cell" $ do
      showCell glider (4, 4) `shouldBe` 'X'
      showCell glider (0, 0) `shouldBe` '.'

    it "show board" $ do
      showBoard glider `shouldBe` "..........\n..........\n....X.....\n..X.X.....\n...XX.....\n..........\n..........\n..........\n..........\n..........\n"

    it "clear baord" $ do
      clearBoard `shouldBe` concat (replicate 100 "\BS")
