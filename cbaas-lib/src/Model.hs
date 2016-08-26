{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# language FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# language QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# language TypeFamilies               #-}

module Model where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad (mzero)
import Data.ByteString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64 as B64
import Data.Complex
import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding
import qualified Data.Aeson as A
import qualified Data.Vector as V
#ifndef __GHCJS__
import Database.Groundhog
#endif
import Database.Groundhog.TH hiding (defaultCodegenConfig)
import Generics.SOP
import GHC.Generics
import GHC.TypeLits
import Text.Read
import Text.ParserCombinators.ReadPrec
import Codec.Picture
import Web.HttpApiData

import EntityID
import Utils

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

data Expr = ELit    Val
          | ELambda Text  Expr
          | EApp    Expr  Expr
          deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance A.ToJSON Expr
instance A.FromJSON Expr

instance NFData Expr


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

data Val = VDouble Double
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
         | VClosure [(Text,Expr)] Expr
         deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)


instance Generics.SOP.Generic Val

newtype PrimComplex = PComplex { getComplex :: Complex Double }
  deriving (Eq, Show, Read, GHC.Generics.Generic, NFData)

instance Ord PrimComplex where
  compare (PComplex p) (PComplex p')
    | realPart p > realPart p' = GT
    | realPart p < realPart p' = LT
    | otherwise = compare (imagPart p) (imagPart p')

instance A.ToJSON PrimComplex where
  toJSON (PComplex c) = A.object ["real" A..= realPart c
                                 ,"imag" A..= imagPart c]

instance A.FromJSON PrimComplex where
  parseJSON (A.Object o) = do
    r <- o A..: "real"
    i <- o A..: "imag"
    return (PComplex (r :+ i))
  parseJSON _ = mzero

-- From basic-sop (not compiling under ghc8)
-- instance NFData Val where
--   rnf = grnf
instance NFData Val


instance A.ToJSON Val
instance A.FromJSON Val

-- | @ModelImage@ is always a base64 encoded tiff with RGBA8 pixels
newtype ModelImage = ModelImage (Image PixelRGBA8)

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

instance Ord ModelImage where
  compare a b = compare (A.encode a) (A.encode b)

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
        let d = decodeImage =<< B64.decode (encodeUtf8 imgString)
        in case d of
          Right di  -> return $ ModelImage (convertRGBA8 di)
          Left  e    -> error $ "DECODING FAILURE: " ++ e -- mzero
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

instance Model.FromVal (Image PixelRGBA8) where
  fromVal (Model.VImage (Model.ModelImage i)) = i
  fromVal x = error $ "Couldn't cast to image: " ++ show x

#ifndef __GHCJS__
mkPersist ghCodeGen [groundhog|
  - primitive: Type
    converter: showReadConverter
  - primitive: Val
    converter: showReadConverter
|]
#endif

instance ToHttpApiData Type where
  toUrlPiece (TFunction a b) = toUrlPiece a <> " -> " <> toUrlPiece b
  toUrlPiece (TTuple a b) = "(" <> toUrlPiece a <> ", " <> toUrlPiece b <> ")"
  toUrlPiece x = T.pack $ show x
  toHeader (TFunction a b) = toHeader a <> " -> " <> toHeader b
  toHeader (TTuple a b) = "(" <> toHeader a <> ", " <> toHeader b <> ")"
  toHeader x = BS.pack $ show x

-- TODO: Parse tuple types
instance FromHttpApiData Type where
  parseUrlPiece x = case T.words x of
    (x: "->" : ys) -> do
      tx   <- parseUrlPiece x
      tRet <- parseUrlPiece (T.unwords ys)
      return $ TFunction tx tRet
    [x] -> note "No read in type" (readMaybe $ T.unpack x)
    _   -> Left "No parse for type"
