{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Job where

import Servant.API
import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Aeson as A
import Data.Text
import Data.UUID.Types
import qualified Data.UUID.Types as UUID
#ifndef __GHCJS__
import Database.Groundhog
#endif
import Database.Groundhog.TH
import Web.HttpApiData
import EntityID

import Model

type JobId  = EntityID Job
type JobMap = EntityMap Job

data Job = Job
  { _jFunName    :: Text
  , _jArg        :: Model.Val
--   , _jReturnType :: Type
  } deriving (Eq, Show)

makeLenses ''Job

instance ToJSON Job where
  toJSON j = object ["function"   .= (j^.jFunName)
                    ,"arg"        .= (j^.jArg)
--                     ,"returntype" .= (^.jReturnType)
                    ]

instance FromJSON Job where
  parseJSON (Object o) = Job
    <$> o .: "function"
    <*> o .: "arg"
--     <*> o .: "returntype"
  parseJSON _ = undefined

data JobResult = JobResult
  { jrVal        :: Model.Val
--   , jrReturnType :: Type
  , jrJob        :: EntityID Job
  } deriving (Eq, Show)

instance ToJSON JobResult where
  toJSON (JobResult v j) = A.object ["value"  .= v
                                    ,"job"    .= j
--                                     ,"returntype" .= t
                                    ]

instance FromJSON JobResult where
  parseJSON (A.Object o) = JobResult
    <$> o .: "value"
    <*> o .: "job"
    -- <*> o .: "returntype"
  parseJSON _ = mzero

#ifndef __GHCJS__
mkPersist defaultCodegenConfig [groundhog|
 - entity: JobResult
|]
#endif
