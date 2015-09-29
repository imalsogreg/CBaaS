{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

------------------------------------------------------------------------------
import Data.Aeson ((.:), (.=), ToJSON(..), FromJSON(..), object)
import Data.UUID (UUID, fromText, toText)
import Servant.API ((:>), (:<|>))

------------------------------------------------------------------------------
type API1 = "v1"  :>  "stimulus" :> CRUD EntityID StimulusResource
                 :<|> "feature"  :> CRUD EntityID Feature


------------------------------------------------------------------------------
data EntityID = LooseID  Text
              | StrictID UUID

instance ToJSON EntityID where
  toJSON (StrictID u) = toText u
  toJSON (LooseID t)  = t

instance FromJSON EntityID where
  parseJSON (String s) = case UUID.fromText s of
    Nothing -> return (LooseID  s)
    Just i  -> return (StrictID i)
  parseJSON _ = mzero

------------------------------------------------------------------------------
type CRUD i v = Capture "id"    i :> Get     '[JSON] v
           :<|> ReqBody '[JSON] v :> Post    '[JSON] i
           :<|> Capture "id"    i :> ReqBody '[JSON] :> Put '[JSON] ()
           :<|> Capture "id"    i :> Delete  '[JSON] ()


------------------------------------------------------------------------------
type StimulusAPI = Capture "id" EntityID :> Get '[JSON] StimulusResource
              :<|> Post '[JSON] StimulusResource

type FeaturesAPI = CRUD EntityID Features

------------------------------------------------------------------------------
data StimulusResource = StimulusResource
  { _srId   :: EntityID
  , _srUrl  :: ResourceURL
  , _srMime :: ResourceMedia
  } deriving (Show, Eq, Ord)


------------------------------------------------------------------------------
instance ToJSON StimulusResource where
  toJSON s = object ["id"   .= UUID.toText (_srId s)
                    ,"url"  .= _srUrl s
                    ,"mime" .= _srMime s
                    ]

------------------------------------------------------------------------------
instance FromJSON StimulusResource where
  parseJSON (Object v) = do
    uuidText <- v .: "id"
    u        <- v .: "url"
    m        <- v .: "mime"
    case fromText uuidText of
      Nothing -> mzero
      Just i  -> return (StimulusResource i u m)

------------------------------------------------------------------------------
data Features = Features
  { featuresId :: UUID
  , featuresMeta :: Value
  , featuresData :: Value
  } deriving (Show, Eq, Ord)

instance ToJSON Features where
  toJSON s = object ["id" .= UUID.toText (_featuresId s)
                    ,"meta" .= _featuresMeta s
                    ,"data" .= _featuresData s
                    ]

------------------------------------------------------------------------------
instance FromJSON Features where
  fromJSON (Object v) = do
    i <- v .: "id"
    m <- v .: "meta"
    d <- v .: "data"
    case UUID.fromText i of
      Nothing -> mzero
      Just i' -> return (Features i' m d)
