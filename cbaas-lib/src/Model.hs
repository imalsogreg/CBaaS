{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# language FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# language QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# language TypeFamilies               #-}

module Model (
  Val(..),
  ToVal(..),
  FromVal(..),
  Expr(..),
  ModelImage(..),
  Prim1(..),
  Prim2(..),
  Type(..),
  WorkerProfile(..),
  WorkerProfileMap,
  WorkerName(..),
  WorkerProfileId,
  module Type
) where

import Codec.Picture
import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad (mzero)
import Data.ByteString
import Data.Aeson ((.=),(.:))
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
import qualified Text.PrettyPrint.HughesPJClass as P
import URI.ByteString
import Web.HttpApiData

import EntityID
import Type
import Utils

class ToVal a where
  toVal :: a -> Val

class FromVal a where
  fromVal :: Val -> a

data WorkerName = WorkerName { unWorkerName :: Text }
  deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance NFData WorkerName

data WorkerProfile = WorkerProfile
  { wpName     :: WorkerName
  , wpFunction :: (Text, Type)
  } deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance NFData WorkerProfile


type WorkerProfileMap = EntityMap WorkerProfile
type WorkerProfileId  = EntityID  WorkerProfile


data Expr a = ELit    a Val
            | EVar    a Text
            | ELambda a Text  (Expr a)
            | ERemote a (WorkerProfileId, WorkerProfile, Text)
            | EApp    a (Expr a)  (Expr a)
            | EPrim1  a Prim1 (Expr a)
            | EPrim2  a Prim2 (Expr a) (Expr a)
            deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance Functor Expr where
  fmap f (ELit a v)  = ELit (f a) v
  fmap f (EVar a n)  = EVar (f a) n
  fmap f (ELambda a n b) = ELambda (f a) n (fmap f b)
  fmap f (ERemote a r) = ERemote (f a) r
  fmap f (EApp a eA eB) = EApp (f a) (fmap f eA) (fmap f eB)
  fmap f (EPrim1 a p e) = EPrim1 (f a) p (fmap f e)
  fmap f (EPrim2 a p eA eB) = EPrim2 (f a) p (fmap f eA) (fmap f eB)

instance A.ToJSON a => A.ToJSON (Expr a)
instance A.FromJSON a => A.FromJSON (Expr a)

instance NFData a => NFData (Expr a)


instance NFData Type


data Prim1 = P1Negate | P1Not
  deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance A.ToJSON Prim1
instance A.FromJSON Prim1
instance NFData Prim1

data Prim2 = P2And | P2Or | P2Sum | P2Map | P2Prod
  deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

instance A.ToJSON Prim2
instance A.FromJSON Prim2
instance NFData Prim2

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
         | VClosure [(Text,Expr Type)] (Expr Type)
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

imageToBytes :: ModelImage -> BS.ByteString
imageToBytes (ModelImage i) = B64.encode . BL.toStrict . encodeTiff $ i

imageFromBytes :: BS.ByteString -> Either String ModelImage
imageFromBytes bs = fmap (ModelImage . convertRGBA8) $ decodeImage =<< B64.decode bs

instance Show ModelImage where
  show = BS.unpack . imageToBytes

instance Read ModelImage where
  readsPrec _ = readModelImage


readModelImage :: ReadS ModelImage
readModelImage s = case imageFromBytes (BS.pack $ Prelude.dropWhile (== ' ') s) of
  Right m -> [(m,"")]
  Left e -> error e

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
  - primitive: Val
    converter: showReadConverter
|]
#endif



instance A.ToJSON WorkerProfile where
  toJSON (WorkerProfile (WorkerName n) (f,t)) =
    A.object ["name" .= n
             ,"function" .= f
             ,"type" .= t
             ]

instance A.FromJSON WorkerProfile where
  parseJSON (A.Object o) = do
    n <- o .: "name"
    f <- o .: "function"
    t <- o .: "type"
    return $ WorkerProfile n (f,t)

instance FromHttpApiData WorkerName where
  parseUrlPiece = Right . WorkerName

instance A.ToJSON WorkerName where
  toJSON (WorkerName n) = A.String n

instance A.FromJSON WorkerName where
  parseJSON (A.String n) = return $ WorkerName n
  parseJSON _ = mzero


parseWorkerProfile :: Query -> Either String WorkerProfile
parseWorkerProfile q = do
  nm  <- note "No WorkerProfile name"     (lookup "name" ps)
  fn  <- note "No WorkerProfile function" (lookup "function" ps)
  fty <- note "No WorkerProfile type" (lookup "type" ps) >>= (note "No parse" . readMaybe . BS.unpack)
  return $ WorkerProfile (WorkerName $ decodeUtf8 nm) (decodeUtf8 fn, fty)
  where ps = queryPairs q

#ifndef __GHCJS__
-- TODO custom WorkerName instance to avoid showing/reading constructors
mkPersist ghCodeGen [groundhog|
  - primitive: WorkerName
    converter: showReadConverter
  - entity: WorkerProfile
|]
#endif
