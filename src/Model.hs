{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.Text
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
import qualified Data.Aeson as A
import qualified Data.Vector as V
import GHC.Generics
import GHC.TypeLits
import Codec.Picture

class ToVal a where
  toVal :: a -> Val

class FromVal a where
  fromVal :: Val -> a

-- -- Singleton types
-- data STy ty where
--   SInt    :: STy Int
--   SBool   :: STy Bool
--   SFloat  :: STy Float
--   SDouble :: STy Double
--   SFun    :: STy a -> STy b -> STy (a -> b)
--   SApp    :: STy a
--   SArray1 :: STy a -> STy (Array 1 a)
--   SArray2 :: STy a -> STy (Array 2 a)
--   SArray3 :: STy a -> STy (Array 3 a)
--   SArray4 :: STy a -> STy (Array 4 a)
--   SText   :: STy Text

-- data Array int a where
--   Array :: 1 -> [a] -> Array 1 a

-- data Expr ty where
--   Lit    :: ty -> TermLit -> Expr ty
--   Lambda :: Variable ty -> Expr ty -> Expr ty
--   App    :: Lambda ty -> Expr ty -> Expr ty


-- TODO, how do you write a language?
data Expr = Expr A.Value
  deriving (Eq, Show)


instance A.ToJSON Expr where
  toJSON (Expr v) = v

instance A.FromJSON Expr where
  parseJSON (A.Object v) = pure (Expr $ A.Object v)
  parseJSON _            = mzero

data Val = VAny    A.Value
         | VDouble Double
         | VText   Text
         | VList   [Val]
         | VProbabilityDistribution [(Val,Double)]
         | VVec1   (V.Vector Double)
         | VVec2   (V.Vector (Double, Double))
         | VVec3   (V.Vector (Double, Double, Double))
         | VMat2   (V.Vector (V.Vector Double))
         | VMat2C  (V.Vector (V.Vector Double),
                    V.Vector (V.Vector Double),
                    V.Vector (V.Vector Double))
         | VImage  ModelImage
         | VClosure [(Text,Val)] Expr
         deriving (Eq, Show, Generic)


instance A.ToJSON Val where
  toJSON (VImage (ModelImage dynImg)) =
    let bytes = case encodeModelImage (ModelImage dynImg) of
          Left e -> "Encoding Error"
          Right b -> b
    in A.object ["tag" A..= ("VImage" :: Text)
                ,"contents" A..= bytes]
  toJSON (VDouble d) = A.object ["tag" A..= ("VDouble" :: Text)
                                ,"contents" A..= d]
  toJSON (VText t) = A.object ["tag" A..= ("VText" :: Text)
                              ,"contents" A..= t]
  toJSON (VMat2 m) = A.object ["tag" A..= ("VMat2" :: Text)
                              ,"contents" A..= m]
  toJSON (VMat2C (r,g,b)) = A.object ["tag" A..= ("VMat2C" :: Text)
                                     ,"contents" A..= (r,g,b)]
  toJSON (VAny v) = v

instance A.FromJSON Val where
  parseJSON (A.Object o) = (do
    tag :: String <- o A..: "tag"
    contents      <- o A..: "contents"
    case tag of
      "VImage" -> case decodeModelImage contents of
        Left _    -> mzero
        Right img -> return (VImage img)
      "VDouble" -> VDouble <$> A.parseJSON contents
      "VText"   -> VText   <$> A.parseJSON contents
      "VMat2"   -> VMat2   <$> A.parseJSON contents
      "VMat2C"  -> VMat2C  <$> A.parseJSON contents
      "VAny"    -> return $ VAny contents
    ) -- <|> pure (VAny (A.Object o))
  -- parseJSON a = return $ VAny a

newtype ModelImage = ModelImage DynamicImage

instance Eq ModelImage where
  a == b =
    encodeModelImage a == encodeModelImage b

instance Show ModelImage where
  show _ = "<CBaaSModelImage>"

encodeModelImage :: ModelImage -> Either String A.Value
encodeModelImage (ModelImage img) =
  (A.String . decodeUtf8 . B64.encode . BL.toStrict) <$> encodeDynamicPng img

decodeModelImage :: A.Value -> Either String ModelImage
decodeModelImage (A.String (s :: Text)) =
  fmap ModelImage $ decodeImage =<< B64.decode (encodeUtf8 s)


instance Model.ToVal Int where
  toVal i = Model.VDouble (realToFrac i)

instance Model.ToVal Double where
  toVal = VDouble

instance Model.ToVal a => Model.ToVal [a] where
  toVal i = Model.VList $ Prelude.map toVal i

instance Model.ToVal Text where
  toVal = VText


instance Model.FromVal Double where
  fromVal (VDouble t) = t
  fromVal e = error $ "Couldn't cast to double: " ++ show e

instance Model.FromVal Text where
  fromVal (VText t) = t
  fromVal e = error $ "Couldn't cast to text: " ++ show e

instance Model.FromVal DynamicImage where
  fromVal (Model.VImage (Model.ModelImage i)) = i
  fromVal x = error $ "Couldn't cast to image: " ++ show x

