{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad (mzero)
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.Complex
import Data.Text
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Generics.SOP
import Generics.SOP.NFData
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

instance NFData Expr where
  rnf (Expr v) = rnf v

instance A.ToJSON Expr where
  toJSON (Expr v) = v

instance A.FromJSON Expr where
  parseJSON (A.Object v) = pure (Expr $ A.Object v)
  parseJSON _            = mzero

data Val = -- VAny    A.Value
        --  |
           VDouble Double
         | VComplex PrimComplex
         | VText Text
         | VImage ModelImage
         | VList   [Val]
         -- | VProbabilityDistribution [(Val,Double)]
         | VVec1   (V.Vector Val)
         | VVec2   (V.Vector (Val, Val))
         | VVec3   (V.Vector (Val, Val, Val))
         | VMat2   (V.Vector (V.Vector Val))
         | VMat2C  (V.Vector (V.Vector Val),
                    V.Vector (V.Vector Val),
                    V.Vector (V.Vector Val))
         -- | VClosure [(Text,Expr)] Expr
         deriving (Eq, Show, GHC.Generics.Generic)

data PrimVal = PrimDouble Double
             | PrimComplex PrimComplex
             | PrimText   Text
             | PrimImage  ModelImage
  deriving (Eq, Show, GHC.Generics.Generic)

instance Generics.SOP.Generic PrimVal

instance Generics.SOP.Generic Val

newtype PrimComplex = PComplex { getComplex :: Complex Double }
  deriving (Eq, Show, GHC.Generics.Generic, NFData)

instance A.ToJSON PrimComplex where
  toJSON (PComplex c) = A.object ["real" A..= realPart c
                                 ,"imag" A..= imagPart c]

instance A.FromJSON PrimComplex where
  parseJSON (A.Object o) = do
    r <- o A..: "real"
    i <- o A..: "imag"
    return (PComplex (r :+ i))
  parseJSON _ = mzero

instance NFData PrimVal where
  rnf = grnf

instance NFData Val where
  rnf = grnf

instance A.ToJSON PrimVal

instance A.ToJSON Val

-- instance A.ToJSON Val where
--   toJSON (VAny v) = v
--   toJSON (VDouble d) = A.object ["tag" A..= ("VDouble" :: Text)]
--   toJSON (VText t) = A.object ["tag" A..= ("VText" :: Text)
--                               ,"contents" A..= t]
--                               ,"contents" A..= d]

--   toJSON (VImage (ModelImage dynImg)) =
--     let bytes = case encodeModelImage (ModelImage dynImg) of
--           Left e -> "Encoding Error"
--           Right b -> b
--     in A.object ["tag" A..= ("VImage" :: Text)
--                 ,"contents" A..= bytes]
--   toJSON (VMat2 m) = A.object ["tag" A..= ("VMat2" :: Text)
--                               ,"contents" A..= m]
--   toJSON (VMat2C (r,g,b)) = A.object ["tag" A..= ("VMat2C" :: Text)
--                                      ,"contents" A..= (r,g,b)]

instance A.FromJSON PrimVal
instance A.FromJSON Val

-- instance A.FromJSON Val where
--   parseJSON (A.Object o) = (do
--     tag :: String <- o A..: "tag"
--     contents      <- o A..: "contents"
--     case tag of
--       "VImage" -> case decodeModelImage contents of
--         Left _    -> mzero
--         Right img -> return (VImage img)
--       "VDouble" -> VDouble <$> A.parseJSON contents
--       "VText"   -> VText   <$> A.parseJSON contents
--       "VMat2"   -> VMat2   <$> A.parseJSON contents
--       "VMat2C"  -> VMat2C  <$> A.parseJSON contents
--       "VAny"    -> return $ VAny contents
--     ) -- <|> pure (VAny (A.Object o))
--   -- parseJSON a = return $ VAny a

newtype ModelImage = ModelImage DynamicImage

instance A.ToJSON ModelImage where
  toJSON i = case encodeModelImage i of
    Left e  -> A.Null
    Right v -> v

instance A.FromJSON ModelImage where
  parseJSON v = case decodeModelImage v of
    Left e  -> mzero
    Right v -> return v

instance Eq ModelImage where
  a == b =
    encodeModelImage a == encodeModelImage b

instance Show ModelImage where
  show _ = "<CBaaSModelImage>"

instance NFData ModelImage where
  rnf (ModelImage img) = rnf img

encodeModelImage :: ModelImage -> Either String A.Value
encodeModelImage (ModelImage img) =
  (A.String . decodeUtf8 . B64.encode . BL.toStrict)
    <$> encodeDynamicPng img

decodeModelImage :: A.Value -> Either String ModelImage
decodeModelImage (A.String (s :: Text)) =
  fmap ModelImage $ decodeImage =<< B64.decode (encodeUtf8 s)


-- instance Model.ToVal Int where
--   toVal i = Model.VDouble (realToFrac i)

-- instance Model.ToVal Double where
--   toVal = VDouble

-- instance Model.ToVal a => Model.ToVal [a] where
--   toVal i = Model.VList $ Prelude.map toVal i

-- instance Model.ToVal Text where
--   toVal = VText


-- instance Model.FromVal Double where
--   fromVal (VDouble t) = t
--   fromVal e = error $ "Couldn't cast to double: " ++ show e

-- instance Model.FromVal Text where
--   fromVal (VText t) = t
--   fromVal e = error $ "Couldn't cast to text: " ++ show e

-- instance Model.FromVal DynamicImage where
--   fromVal (Model.VImage (Model.ModelImage i)) = i
--   fromVal x = error $ "Couldn't cast to image: " ++ show x
