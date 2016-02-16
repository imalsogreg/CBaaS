{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Worker where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, mzero)
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.Map as Map
import Data.Text hiding (map, filter)
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding
import Data.UUID
import qualified Data.UUID as UUID
import Data.UUID.V4
import GHC.Generics
import qualified Network.WebSockets as WS
import Servant.API
import URI.ByteString
import Web.HttpApiData

import Job
import Model
import Browser

data Worker = Worker
  { _wProfile :: WorkerProfile
  , _wID      :: WorkerID
  , _wConn    :: WS.Connection
  , _wJobQueue :: TChan (JobID, Maybe BrowserID, Job)
  } deriving (Generic)


newtype WorkerMap = WorkerMap (Map.Map WorkerID WorkerProfile)

instance ToJSON WorkerMap where
  toJSON (WorkerMap w) = toJSON $ Map.mapKeys (UUID.toText . unWorkerID) w

data WorkerProfile = WorkerProfile
  { wpName         :: WorkerName
  , wpFunctionName :: Text
  , wpTags         :: [Text]
  } deriving (Eq, Ord, Show)

instance ToJSON WorkerProfile where
  toJSON (WorkerProfile (WorkerName n) f t) =
    A.object ["name" .= n
             ,"function" .= f
             ,"tags" .= t]

instance FromJSON WorkerProfile where
  parseJSON (A.Object o) = WorkerProfile
    <$> o .: "name"
    <*> o .: "function"
    <*> o .: "tags"

newtype WorkerID = WorkerID { unWorkerID :: UUID }
  deriving (Eq, Ord, Show)

instance ToJSON WorkerID where
  toJSON (WorkerID i) = A.String $ UUID.toText i

instance FromHttpApiData WorkerID where
  parseUrlPiece t = WorkerID <$>
    Worker.note "Bad UUID parse" (UUID.fromText t)

instance ToHttpApiData WorkerID where
  toUrlPiece (WorkerID u) = UUID.toText u

instance FromJSON WorkerID where
  parseJSON (A.String s) = case UUID.fromText s of
    Nothing -> mzero
    Just i  -> return $ WorkerID i
  parseJSON _ = mzero

newtype WorkerName = WorkerName { unWN :: Text }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON WorkerName where
  toJSON (WorkerName n) = A.String n

instance FromJSON WorkerName where
  parseJSON (A.String n) = return $ WorkerName n
  parseJSON _ = mzero


parseWorkerProfile :: Query -> Either String WorkerProfile
parseWorkerProfile q = do
  nm <- Worker.note "No WorkerProfile name"     $ lookup "name" ps
  fn <- Worker.note "No WorkerProfile function" $ lookup "function" ps
  return $ WorkerProfile (WorkerName $ decodeUtf8 nm)
           (decodeUtf8 fn)
           (map decodeUtf8 . map snd . filter ((== "tag") . fst) $ ps)
  where ps = queryPairs q


note :: e -> Maybe a -> Either e a
note err Nothing  = Left err
note _   (Just a) = Right a
