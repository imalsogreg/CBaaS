{-# LANGUAGE OverloadedStrings #-}

module Browser where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Map as Map
import Data.UUID
import Data.UUID.V4
import qualified Network.WebSockets as WS
import qualified Servant.API as Servant
import Web.HttpApiData
import BrowserProfile
import EntityID
import Job

data Browser = Browser
  { bID         :: BrowserProfileId
  , bConn       :: WS.Connection
  , bJobResults :: TChan JobResult
  }

type BrowserMap = Map.Map BrowserProfileId Browser
