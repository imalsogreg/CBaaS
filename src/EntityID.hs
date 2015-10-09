{-# LANGUAGE OverloadedStrings #-}

module EntityID where

import           Control.Monad (mzero)
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Monoid
import           Data.Text (unpack)
import qualified Data.UUID as UUID
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Servant.API

------------------------------------------------------------------------------
-- | A type wrapper for Entity ID tags. We wrap the UUID in our own type
--   so that we can manually define how to convert id's into JSON data
newtype EntityID = EntityID { unID :: UUID.UUID }
  deriving (Show, Read, Eq, Ord)


------------------------------------------------------------------------------
-- | Convert an ID to JSON. Construct a JSON String value from the
--   textual representation of the UUID
instance ToJSON EntityID where
  toJSON (EntityID u) = A.String (UUID.toText u)


------------------------------------------------------------------------------
-- | Convert a JSON string into an ID. On successful parsing of the UUID's
--   string, return our wrapped ID type. If string parsing fails or ther
--   JSON type was not a string, ID parsing fails (`mzero`)
instance FromJSON EntityID where
  parseJSON (A.String s) = case UUID.fromText s of
    Nothing -> mzero
    Just i  -> return (EntityID i)
  parseJSON _ = mzero


instance ToField EntityID where
  toField (EntityID i) = toField i

instance FromField EntityID where
  fromField a b = EntityID <$> fromField a b

instance FromText EntityID where
  fromText t = EntityID <$> UUID.fromText t

instance FromFormUrlEncoded EntityID where
  fromFormUrlEncoded [("id", t)] =
    case UUID.fromText t of
      Nothing -> Left . unpack $ "Id parse error on text:" <> t
      Just i  -> Right $ EntityID i
  fromFormUrlEncoded _ = Left "No query param 'id'"

instance ToRow EntityID where
  toRow i = [toField i]

instance FromRow EntityID where
  fromRow = field

