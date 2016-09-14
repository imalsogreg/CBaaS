{-# LANGUAGE CPP     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}


module EntityID where

import           Control.DeepSeq
import           Control.Monad         (mzero)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson            as A
import qualified Data.Aeson.Parser     as A
import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map              as Map
import           Data.Maybe            (fromJust)
import           Data.Monoid
import           Data.Proxy
import           Data.Text             (Text, unpack)
import qualified Data.UUID.Types             as UUID
#ifndef __GHCJS__
import           Database.Groundhog
import           Database.Groundhog.Generic
#endif
import           Database.Groundhog.Core
import           Database.Groundhog.TH
import           GHC.Generics
import           Servant.API
import           Web.HttpApiData
import           Utils (note)

------------------------------------------------------------------------------
-- | A type wrapper for Entity ID tags. We wrap the UUID in our own type
--   so that we can manually define how to convert id's into JSON data
newtype EntityID a = EntityID { unID :: UUID.UUID }
  deriving (Show, Read, Eq, Ord)


newtype EntityMap a = EntityMap { unEntityMap :: Map.Map (EntityID a) a }
                      deriving (Eq, Ord, Generic)

instance ToJSON a => ToJSON (EntityMap a) where
  toJSON (EntityMap m) = A.toJSON (Map.mapKeys (UUID.toText . unID) m)

instance FromJSON a => FromJSON (EntityMap a) where
  parseJSON (A.Object o) =
    fmap (EntityMap . Map.fromList) $
    traverse (\(k,v) -> ((,) . EntityID) <$> parseUUID k <*> A.parseJSON v)
    (HM.toList o)

-- parseUUID :: Text -> A.Parser UUID.UUID
parseUUID t = case UUID.fromText t of
  Just u  -> return u
  Nothing -> mzero

instance NFData (EntityID a) where
  rnf (EntityID x) = rnf x

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

#ifndef __GHCJS__
instance PrimitivePersistField (EntityID a) where
  toPrimitivePersistValue (EntityID uuid) = PersistString $ show uuid
  fromPrimitivePersistValue s =
    let pDecodeString x = case UUID.fromString x of
            Just i -> EntityID i
            Nothing -> error $ "Error calling fromString on " ++ show x
    in case s of
      PersistString str -> pDecodeString str
      PersistByteString str -> pDecodeString $ BS.unpack str

instance PersistField (EntityID a) where
  persistName _ = "uuid"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType db _ = case backendName db of
    "postgresql" -> DbTypePrimitive (DbOther $ OtherTypeDef [Left "uuid"]) False Nothing Nothing
    _ -> DbTypePrimitive DbString False Nothing Nothing
#endif

-- instance ToField (EntityID a) where
--   toField (EntityID i) = toField i

-- instance FromField (EntityID a) where
--   fromField a b = EntityID <$> fromField a b

instance FromHttpApiData (EntityID a) where
  parseUrlPiece t = EntityID <$> note "Bad UUID decode" (UUID.fromText t)

instance ToHttpApiData (EntityID a) where
  toUrlPiece (EntityID u) = UUID.toText u

{-
instance FromFormUrlEncoded (EntityID a) where
  fromFormUrlEncoded [("id", t)] =
    case UUID.fromText t of
      Nothing -> Left . unpack $ "Id parse error on text:" <> t
      Just i  -> Right $ EntityID i
  fromFormUrlEncoded _ = Left "No query param 'id'"
-}

-- instance ToRow (EntityID a) where
--   toRow i = [toField i]

-- instance FromRow (EntityID a) where
--   fromRow = field
