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

import Model

newtype JobID = JobID { _unJobID :: UUID }
  deriving (Eq, Show, Ord)

instance ToJSON JobID where
  toJSON (JobID uu) = String $ UUID.toText uu

instance FromJSON JobID where
  parseJSON (String s) = maybe mzero (return . JobID) (UUID.fromText s)
  parseJSON _          = mzero

instance FromHttpApiData JobID where
  parseUrlPiece t = JobID <$> note "Bad UUID decode" (UUID.fromText t)
note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a

instance ToHttpApiData JobID where
  toUrlPiece (JobID u) = UUID.toText u

makeLenses ''JobID

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
  , jrJob    :: JobID
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

