{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Job where

import Servant.API
import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.Aeson as A
import Data.Text
import Data.UUID
import qualified Data.UUID as UUID
import Web.HttpApiData
import EntityID

import Model

type JobMap = EntityMap Job

data Job = Job
  { -- _jID      :: JobID
    _jFunName :: Text
  , _jArg     :: Model.Val
  } deriving (Eq, Show)

makeLenses ''Job

instance ToJSON Job where
  toJSON j = object [ -- "id"       .= (j^.jID)
                     "function" .= (j^.jFunName)
                    ,"arg"     .= (j^.jArg)]

instance FromJSON Job where
  parseJSON (Object o) = Job
--     <$> o .: "id"
    <$> o .: "function"
    <*> o .: "arg"
  parseJSON _ = undefined

data JobResult = JobResult
  { jrVal    :: Model.Val
  -- , jrWorker :: WorkerID
  , jrJob    :: EntityID Job
  } deriving (Eq, Show)

instance ToJSON JobResult where
  toJSON (JobResult v j) = A.object ["value"  .= v
    --                                  ,"worker" .= w
                                      ,"job"    .= j
                                      ]

instance FromJSON JobResult where
  parseJSON (A.Object o) = JobResult
    <$> o .: "value"
    -- <*> o .: "worker"
    <*> o .: "job"
  parseJSON _ = mzero

