{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

------------------------------------------------------------------------------
import           Control.Monad (mzero)
import qualified Data.Aeson    as A
import           Data.Aeson    ((.:), (.=), ToJSON(..), FromJSON(..), Value)
import qualified Data.ByteString as BS
import           Data.Monoid   ((<>))
import           Data.Text     (Text, pack, unpack)
import qualified Data.UUID     as UUID
import           Data.UUID     (UUID, fromByteString, toByteString)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics
import           EntityID
import           Permissions
import           User
import           Servant.API



------------------------------------------------------------------------------
-- | A stimulus hosted on the site
data Stimulus = SLink Text
                -- ^ A URL for a stimulus.
                --   Learn its type from Content-Type header
              | SImage Text
                -- ^ ByteString of an image.
                --   Learn its type from the magic bits
              | SString Text
                -- ^ Some body of text
              | SJson Value
                -- ^ A Generic JSON value. Maximum flexibility, but we won't
                --   be able to make much use of them on the site
  deriving (Eq, Show, Generic)


------------------------------------------------------------------------------
-- | A Stimulus Resource
--   We include an ID tag for referencing this stimulus from feature vectors
--   derived from it. Mime type is provided as a sanity check and to allow
--   clients to avoid downloading certain stimulus types if needed
data StimulusResource = StimulusResource
  { -- _srId   :: EntityID
    -- -- ^ Database key for stimulus
    -- _srPerm :: Ownership
    -- -- ^ Permissions for owner, group, and public
    _srMeta :: MetaData
    -- ^ MetaData (this should not be delivered to classifiers),
    -- an unstructured JSON Object
  , _srStim :: Stimulus
    -- ^ The data available to a classifier
  } deriving (Show, Eq, Generic)


------------------------------------------------------------------------------
-- | Feature data, completely open-ended (any JSON data), but tagged with an
--   ID and metadata
data Features = Features
  { -- _featuresId   :: EntityID
    _featuresMeta :: A.Value
  , _featuresData :: FeatureData
  } deriving (Show, Eq, Generic)


------------------------------------------------------------------------------
-- | Various forms of feature vectors
data FeatureData = FBool   Bool
                 | FString Bool
                 | FJSON   Value
                 deriving (Show, Eq, Generic)


------------------------------------------------------------------------------
instance A.ToJSON   StimulusResource where
instance A.FromJSON StimulusResource where

instance ToJSON   Features where
instance FromJSON Features where

instance ToJSON   FeatureData where
instance FromJSON FeatureData where

instance ToField FeatureData where
  toField = toField . A.encode

instance FromField FeatureData where
  fromField a b = do
    j <- fromField a b
    case A.decode j of
      Just v  -> return v
      Nothing -> mzero

instance ToRow StimulusResource where
  toRow (StimulusResource u m) =
    [toField u, toField m]

instance ToRow Features where
  toRow (Features m d) = [toField m, toField d]

instance FromRow StimulusResource where
  fromRow = StimulusResource <$> field <*> field

instance FromRow Features where
  fromRow = Features <$> field <*> field
instance ToField Stimulus where
  toField s = toField $ A.encode s

instance FromField Stimulus where
  fromField a b = do
    j <- fromField a b
    case A.decode j of
      Just v  -> return v
      Nothing -> mzero

instance ToJSON   Stimulus where
instance FromJSON Stimulus where

newtype MetaData = MetaData { unMeta :: Value }
                 deriving (Eq, Show, ToJSON, FromJSON)

instance ToField   MetaData where
  toField (MetaData v) = toField v

instance FromField MetaData where
  fromField a b = MetaData <$> fromField a b

