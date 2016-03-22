{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}


module EntityID where

import           Control.Monad (mzero)
import qualified Data.Aeson as A
import           Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (unpack)
import qualified Data.UUID as UUID
-- import           Database.PostgreSQL.Simple.ToField
-- import           Database.PostgreSQL.Simple.FromField
-- import           Database.PostgreSQL.Simple.FromRow
-- import           Database.PostgreSQL.Simple.ToRow
import           Database.Groundhog.TH
import           GHC.Generics
import           Servant.API
import           Web.HttpApiData

------------------------------------------------------------------------------
-- | A type wrapper for Entity ID tags. We wrap the UUID in our own type
--   so that we can manually define how to convert id's into JSON data
newtype EntityID a = EntityID { unID :: UUID.UUID }
  deriving (Show, Read, Eq, Ord)

newtype EntityMap a = EntityMap { unEntityMap :: Map.Map (EntityID a) a }
                      deriving (Eq, Ord, Generic)

instance ToJSON a => ToJSON (EntityMap a) where
  toJSON (EntityMap m) = A.toJSON (Map.mapKeys (UUID.toText . unID) m)


------------------------------------------------------------------------------
-- | Convert an ID to JSON. Construct a JSON String value from the
--   textual representation of the UUID
instance ToJSON (EntityID a) where
  toJSON (EntityID u) = A.String (UUID.toText u)


------------------------------------------------------------------------------
-- | Convert a JSON string into an ID. On successful parsing of the UUID's
--   string, return our wrapped ID type. If string parsing fails or ther
--   JSON type was not a string, ID parsing fails (`mzero`)
instance FromJSON (EntityID a) where
  parseJSON (A.String s) = case UUID.fromText s of
    Nothing -> mzero
    Just i  -> return (EntityID i)
  parseJSON _ = mzero


-- instance ToField (EntityID a) where
--   toField (EntityID i) = toField i

-- instance FromField (EntityID a) where
--   fromField a b = EntityID <$> fromField a b

instance FromHttpApiData (EntityID a) where
  parseUrlPiece t = EntityID <$> note "Bad UUID decode" (UUID.fromText t)

instance ToHttpApiData (EntityID a) where
  toUrlPiece (EntityID u) = UUID.toText u

instance FromFormUrlEncoded (EntityID a) where
  fromFormUrlEncoded [("id", t)] =
    case UUID.fromText t of
      Nothing -> Left . unpack $ "Id parse error on text:" <> t
      Just i  -> Right $ EntityID i
  fromFormUrlEncoded _ = Left "No query param 'id'"

-- instance ToRow (EntityID a) where
--   toRow i = [toField i]

-- instance FromRow (EntityID a) where
--   fromRow = field

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a

ghCodeGen :: CodegenConfig
ghCodeGen = defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle }
