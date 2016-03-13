{-# LANGUAGE OverloadedStrings #-}

module BrowserProfile where


import qualified Data.Aeson as A
import qualified Data.Map as Map
import           Data.UUID
import           Data.UUID.V4
import qualified Servant.API as Servant
import           Web.HttpApiData

import           EntityID
import           Job

data BrowserProfile = BrowserProfile
  { bID         :: EntityID BrowserProfile
  }

type BrowserProfileMap = EntityMap BrowserProfile
type BrowserProfileId  = EntityID  BrowserProfile

