{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# language QuasiQuotes       #-}
{-# language TypeFamilies       #-}
{-# language GADTs       #-}
{-# language FlexibleInstances       #-}

module Model where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad (mzero)
import Data.ByteString
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as B64
import Data.Complex
import Data.Text
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Database.Groundhog
import Database.Groundhog.TH
import Generics.SOP
import Generics.SOP.NFData
import GHC.Generics
import GHC.TypeLits
import Text.Read
import Text.ParserCombinators.ReadPrec
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

data Type = TDouble
          | TComplex
          | TText
          | TModelImage
          | TList [Type]
          | TVec
          | TTuple Type Type
          | TFunction Type Type
  deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance A.ToJSON Type

instance A.FromJSON Type

data Val = -- VAny    A.Value
        --  |
           VDouble Double
         | VComplex PrimComplex
         | VText Text
         | VImage ModelImage
         -- TODO: I'm having trouble getting the recursive values into groundhog
         | VList   [Val]
         | VProbabilityDistribution [(Val,Double)]
         | VVec1   (V.Vector Val)
         | VVec2   (V.Vector (Val, Val))
         | VVec3   (V.Vector (Val, Val, Val))
         | VMat2   (V.Vector (V.Vector Val))
         | VMat2C  (V.Vector (V.Vector Val),
                    V.Vector (V.Vector Val),
                    V.Vector (V.Vector Val))
         -- | VClosure [(Text,Expr)] Expr
         deriving (Eq, Show, Read, GHC.Generics.Generic)


instance Generics.SOP.Generic Val

newtype PrimComplex = PComplex { getComplex :: Complex Double }
  deriving (Eq, Show, Read, GHC.Generics.Generic, NFData)

instance A.ToJSON PrimComplex where
  toJSON (PComplex c) = A.object ["real" A..= realPart c
                                 ,"imag" A..= imagPart c]

instance A.FromJSON PrimComplex where
  parseJSON (A.Object o) = do
    r <- o A..: "real"
    i <- o A..: "imag"
    return (PComplex (r :+ i))
  parseJSON _ = mzero

instance NFData Val where
  rnf = grnf


instance A.ToJSON Val

instance A.FromJSON Val

-- | @ModelImage@ is always a base64 encoded tiff with RGBA16 pixels
newtype ModelImage = ModelImage (Image PixelRGBA16)

instance Show ModelImage where
  show = show . A.toJSON

instance Read ModelImage where
  readPrec = do
    str <- look
    case A.decode (BL.pack str) of
      Nothing -> pfail
      Just mi -> return mi

instance Eq ModelImage where
  a == b =
    A.toJSON a == A.toJSON b

instance NFData ModelImage where
  rnf (ModelImage img) = rnf img

instance A.ToJSON ModelImage where
  toJSON (ModelImage img) =
    A.object ["tag"      A..= ("ModelImage" :: String)
             ,"contents" A..= imgString]
    where imgString = A.String . decodeUtf8
                    . B64.encode . BL.toStrict
                    . encodeTiff $ img

instance A.FromJSON ModelImage where
  parseJSON (A.Object o) = do
    t <- o A..: "tag"
    c <- o A..: "contents"
    case (t,c) of
      ("ModelImage" :: String, imgString) ->
        let d = decodeTiff =<< B64.decode (encodeUtf8 imgString)
        in case d of
          Right (ImageRGBA16 tiff) -> return $ ModelImage tiff
          Left  e    -> mzero
      _ -> mzero

data Tensor = Tensor
  { tDoubleShape :: [Int]
  , tDoubleElems :: V.Vector Double
  }


instance Model.ToVal Int where
  toVal i = Model.VDouble (realToFrac i)

instance Model.ToVal Double where
  toVal = VDouble

-- instance Model.ToVal a => Model.ToVal [a] where
--   toVal i = Model.VList $ Prelude.map toVal i

instance Model.ToVal Text where
  toVal = VText


instance Model.FromVal Double where
  fromVal (VDouble t) = t
  fromVal e = error $ "Couldn't cast to double: " ++ show e

instance Model.FromVal Text where
  fromVal (VText t) = t
  fromVal e = error $ "Couldn't cast to text: " ++ show e

-- instance Model.FromVal DynamicImage where
--   fromVal (Model.VImage (Model.ModelImage i)) = i
--   fromVal x = error $ "Couldn't cast to image: " ++ show x

mkPersist defaultCodegenConfig [groundhog|
  - primitive: Type
    representation: showread
  - primitive: Val
    representation: showread
|]
