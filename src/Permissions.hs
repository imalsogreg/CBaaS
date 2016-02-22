{-# LANGUAGE DeriveGeneric #-}

module Permissions where

import Data.Aeson
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import GHC.Generics
import EntityID
import User

data Ownership = Ownership
  { _permOwner      :: EntityID User
  , _permGroup      :: EntityID Group
  , _permOwnerRead  :: Bool
  , _permOwnerWrite :: Bool
  , _permGroupRead  :: Bool
  , _permGroupWrite :: Bool
  , _publicRead     :: Bool
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON   Ownership where
instance FromJSON Ownership where

instance ToRow Ownership where
  toRow (Ownership o g oR oW gR gW pR) =
    [toField o, toField g, toField oR, toField oW
    ,toField gR, toField gW, toField pR
    ]

instance FromRow Ownership where
  fromRow = Ownership <$> field <*> field <*> field
            <*> field <*> field <*> field <*> field

instance FromField Ownership where
  fromField a b = read <$> fromField a b
