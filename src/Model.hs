{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Model where

import Data.Text
import GHC.TypeLits
import qualified Data.Aeson as A

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

data Val = VAny A.Value
  deriving (Eq, Show)
