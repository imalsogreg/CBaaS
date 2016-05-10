{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

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
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH

import Browser
import BrowserProfile
import EntityID
import Job
import Model
import RemoteFunction
import WorkerProfile

data Worker = Worker
  { _wProfile :: WorkerProfile
  , _wID      :: EntityID WorkerProfile
  , _wConn    :: WS.Connection
  , _wJobQueue :: TChan (EntityID Job, Job)
  } deriving (Generic)
