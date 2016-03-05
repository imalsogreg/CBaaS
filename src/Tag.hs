{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}

module Tag where

import Data.Aeson
import Data.Text
import GHC.Generics

newtype Tag = Tag { _unTag :: Text }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
