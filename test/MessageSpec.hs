module MessageSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Codec.Picture
import Message
import Model

spec = do
  describe "Message test" $ do
    it "is checked by hspec" $ do
      1 `shouldBe` 1
