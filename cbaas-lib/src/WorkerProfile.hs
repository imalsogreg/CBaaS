{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

module WorkerProfile where

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
import Data.UUID.Types
import qualified Data.UUID.Types as UUID
-- import Data.UUID.V4
import GHC.Generics
-- import qualified Network.WebSockets as WS
import Servant.API
import URI.ByteString
import Web.HttpApiData
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH

import BrowserProfile
import EntityID
import Job
import Model
import RemoteFunction

type WorkerProfileMap = EntityMap WorkerProfile
type WorkerProfileId  = EntityID  WorkerProfile


data WorkerProfile = WorkerProfile
  { wpName         :: WorkerName
  , wpFunctionName :: Text
  } deriving (Eq, Ord, Show)

instance ToJSON WorkerProfile where
  toJSON (WorkerProfile (WorkerName n) f) =
    A.object ["name" .= n
             ,"function" .= f
             ]

instance FromJSON WorkerProfile where
  parseJSON (A.Object o) = WorkerProfile
    <$> o .: "name"
    <*> o .: "function"

instance FromHttpApiData WorkerName where
  parseUrlPiece = Right . WorkerName

data WorkerName = WorkerName { unWN :: Text }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON WorkerName where
  toJSON (WorkerName n) = A.String n

instance FromJSON WorkerName where
  parseJSON (A.String n) = return $ WorkerName n
  parseJSON _ = mzero


parseWorkerProfile :: Query -> Either String WorkerProfile
parseWorkerProfile q = do
  nm <- note "No WorkerProfile name"     $ lookup "name" ps
  fn <- note "No WorkerProfile function" $ lookup "function" ps
  return $ WorkerProfile (WorkerName $ decodeUtf8 nm)
           (decodeUtf8 fn)
  where ps = queryPairs q

-- TODO: fix field names, to snake case
ghCodegenConfig = defaultCodegenConfig

mkPersist defaultCodegenConfig [groundhog|
  - primitive: WorkerName
    converter: (unWN, WorkerName)
  - entity: WorkerProfile
|]
