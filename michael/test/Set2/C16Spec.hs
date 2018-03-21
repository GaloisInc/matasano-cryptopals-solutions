module Set2.C16Spec where

import Test.Hspec

import Set2.C16

spec :: Spec
spec = do
  describe "tamperedBlocks" $ do
    it "can escalate to admin" $ do
      isAdmin $ decode $ concat tamperedBlocks
      `shouldBe`
      True
