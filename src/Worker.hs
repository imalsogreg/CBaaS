{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Worker where

import Control.Concurrent.STM.TChan
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson as A
import Data.Text
import Data.UUID
import GHC.Generics
import qualified Network.WebSockets as WS

import Job
import Model

data Worker = Worker
  { wName        :: WorkerName -- ^ Name of worker purely for display (may specify function and physical hardware)
  , wID          :: WorkerID   -- ^ UUID of worker
  , wConn        :: WS.Connection -- ^ Connection from worker object to worker client process
  , wJobQueue    :: TChan Job -- ^ Worker's individual job request channel
  , wResultQueue :: TChan JobResult -- ^ Worker-shared channel of job results
    -- TODO what other features to track?
  } deriving (Generic)

newtype WorkerID = WorkerID { unWorkerID :: UUID }
  deriving (Eq, Ord, Show)

newtype WorkerName = WorkerName { unWN :: Text }
  deriving (Eq, Ord, Show, Generic)


data JobResult = JobResult
  { jrVal    :: Model.Val
  , jrWorker :: WorkerID
  } deriving (Eq, Show)

-- instance ToJSON WorkerName where
--   toJSON (WorkerName n) = A.String n

-- instance FromJSON WorkerName where
--   parseJSON (A.String s) = return $ WorkerName s
--   parseJSON _            = mzero

-- instance ToJSON Worker where
--   toJSON (Worker n pID) = A.object ["name" .= n]

-- instance FromJSON Worker where
--   parseJSON (A.Object o) =
--     Worker <$> o .: "name"
--   parseJSON _ = mzero
