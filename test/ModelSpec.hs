{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModelSpec where

import Control.Applicative
import Codec.Picture
import qualified Data.Aeson as A
import Data.Complex
import qualified Data.HashMap.Strict as HM
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import GHC.Generics
import GHC.Word
import Generics.SOP.Arbitrary
import Generics.SOP
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Model

spec = do
  describe "Encoding and decoding"
    $ modifyMaxSize (const 10)
    $ modifyMaxSuccess (const 50) $ do
      it "val should round-trip aeson" $  property $ \(v :: Val) ->
        A.decode (A.encode v) == Just v

instance Arbitrary PrimVal where
  arbitrary = garbitrary

instance Arbitrary Val where
  arbitrary = sized arbVal

arbVal :: Int -> Gen Val
arbVal 0 = oneof [ VDouble  <$> arbitrary
                 , VComplex <$> arbitrary
                 , VText    <$> arbitrary
                 , VImage   <$> arbitrary
                 ]
arbVal n = do
  (Positive m) <- arbitrary
  let n' = n `div` (m + 1)
  oneof [ (VList . V.toList) <$> V.replicateM n (arbVal n')
        , VVec1 <$> V.replicateM n' (arbVal m)
        , fmap VVec2 $ do
           x <- (V.replicateM (div n 4) (arbVal (n `div` 4)))
           y <- (V.replicateM (div n 4) (arbVal (n `div` 4)))
           return (V.zip x y)
        , fmap (VVec3 . V.fromList) $ do
           x <- (vectorOf (div n 2) (arbVal (div n 2)))
           y <- (vectorOf (div n 2) (arbVal (div n 2)))
           z <- (vectorOf (div n 2) (arbVal (div n 2)))
           return (zip3 x y z)
        ]

instance Arbitrary PrimComplex where
  arbitrary = fmap PComplex $ liftA2 (:+) arbitrary arbitrary

instance Arbitrary A.Value where
  arbitrary = oneof
    [ fmap (A.String . T.pack) arbitrary
    , fmap (A.Number . realToFrac) (arbitrary :: Gen Double)
    , return A.Null
    , fmap (A.Object . HM.fromList) (zip <$> arbitrary <*> arbitrary)
    , fmap (A.Array . V.fromList) arbitrary
    ]

instance Arbitrary Expr where
  arbitrary = fmap Expr arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

genImg :: (Pixel a, Arbitrary a)
       => Proxy a
       -> Int
       -> Int
       -> Gen (Image a)
genImg _ wid hgt = do
  f :: Fun (Int,Int) a <- arbitrary
  let img = generateImage (curry (apply f)) wid hgt
  return img

r16 :: (Word16,Word16)
r16 = (minBound,maxBound)

r8 :: (Word8,Word8)
r8 = (minBound,maxBound)

rF :: (Float, Float)
rF = (0,1)

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = fmap V.fromList arbitrary

instance Arbitrary PixelYA8 where
  arbitrary = liftA2 PixelYA8 (choose r8) (choose r8)

instance Arbitrary PixelYA16 where
  arbitrary = liftA2 PixelYA16 (choose r16) (choose r16)

instance Arbitrary PixelRGB8 where
  arbitrary = liftA3 PixelRGB8 (choose r8) (choose r8) (choose r8)

instance Arbitrary PixelRGB16 where
  arbitrary = liftA3 PixelRGB16 (choose r16) (choose r16) (choose r16)

instance Arbitrary PixelRGBF where
  arbitrary = liftA3 PixelRGBF (choose rF) (choose rF) (choose rF)

liftA4 :: Applicative m
       => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

instance Arbitrary PixelRGBA8 where
  arbitrary = liftA4 PixelRGBA8 (choose r8) (choose r8) (choose r8) (choose r8)

instance Arbitrary PixelRGBA16 where
  arbitrary = liftA4 PixelRGBA16 (choose r16) (choose r16) (choose r16) (choose r16)

instance Arbitrary PixelCMYK8 where
  arbitrary = liftA4 PixelCMYK8 (choose r8) (choose r8) (choose r8) (choose r8)

instance Arbitrary PixelCMYK16 where
  arbitrary = liftA4 PixelCMYK16 (choose r16) (choose r16) (choose r16) (choose r16)

instance Arbitrary PixelYCbCr8 where
  arbitrary = liftA3 PixelYCbCr8 (choose r8) (choose r8) (choose r8)


instance Arbitrary ModelImage where
  arbitrary = genModelImage

genModelImage :: Gen ModelImage
genModelImage = do
    x <- choose (10 :: Int, 200)
    y <- choose (10 :: Int, 200)
    dynImg <- oneof
      [ -- ImageY8     <$> genImg (Proxy :: Proxy Pixel8)      x y
      -- , ImageY16    <$> genImg (Proxy :: Proxy Pixel16)     x y
      -- , ImageYF     <$> genImg (Proxy :: Proxy PixelF)      x y
      -- , ImageYA8    <$> genImg (Proxy :: Proxy PixelYA8)    x y
      -- , ImageYA16   <$> genImg (Proxy :: Proxy PixelYA16)   x y
        ImageRGB8   <$> genImg (Proxy :: Proxy PixelRGB8)   x y
      , ImageRGB16  <$> genImg (Proxy :: Proxy PixelRGB16)  x y
      -- , ImageRGBF   <$> genImg (Proxy :: Proxy PixelRGBF)   x y
      , ImageRGBA8  <$> genImg (Proxy :: Proxy PixelRGBA8)  x y
      , ImageRGBA16 <$> genImg (Proxy :: Proxy PixelRGBA16) x y
      -- , ImageYCbCr8 <$> genImg (Proxy :: Proxy PixelYCbCr8) x y
      -- , ImageCMYK8  <$> genImg (Proxy :: Proxy PixelCMYK8)  x y
      -- , ImageCMYK16 <$> genImg (Proxy :: Proxy PixelCMYK16) x y
      ]
    return (ModelImage dynImg)
