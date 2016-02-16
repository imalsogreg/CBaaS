{-# LANGUAGE DeriveGeneric #-}

module ModelSpec where

import GHC.Generics
import Generics.SOP.Arbitrary
import Generics.SOP
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Hspec

spec = do
  describe "Simple test" $ do
    it "is seen by hspec" $ do
      True `shouldBe` True

instance Arbitrary ModelImage where
  arbitrary = do
    xSize <- condition (< 2000) arbitrary
    ySize <- condition (< 2000) arbitrary
    fmt <- choose [Jpeg, Png, Gif, BMP]
    case fmt of
      Jpeg -> something
