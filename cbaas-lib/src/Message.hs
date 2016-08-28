{-# LANGUAGE DeriveGeneric #-}

module Message where

------------------------------------------------------------------------------
import qualified Data.Aeson as A
import           GHC.Generics
------------------------------------------------------------------------------
import           Model
import           BrowserProfile
import           EntityID
import           Job
import           WorkerProfile

------------------------------------------------------------------------------
-- | 'BrowserMessage's go between browser and CBaaS server
data BrowserMessage = WorkerJoined (EntityID WorkerProfile) WorkerProfile
                      -- ^ Informing the browser that a worker has joined
                    | WorkerLeft   (EntityID WorkerProfile)
                      -- ^ Informing the browser that a worker has left
                    | JobFinished  (EntityID Job)
                      -- ^ Informing the browser of job completion - usually
                      --   for jobs that this browser requested
                    | JobStatusUpdate (EntityID Job, JobResult)
                      -- ^ Informing the browser of a status-update on a
                      --   job
                    | SetBrowserID (BrowserProfileId)
                      -- ^ Inform the browser of its ID number
                      --   TODO: should this be asynchronous information?
                      --   TODO: Does it need to be validated or enforced
                      --         somehow?
  deriving (Eq, Show, Generic)

instance A.ToJSON BrowserMessage
instance A.FromJSON BrowserMessage

------------------------------------------------------------------------------
-- | 'WorkerMessage's go between CBaaS server and online workers
data WorkerMessage = JobRequested
                     (EntityID Job, Job)
                     -- ^ Informs a worker that a user has requested a job.
                     --   CBaaS server is
                     --   responsible for ensuring that job requests are
                     --   filtered by function name, argument type,
                     --   and the worker's willingness to do jobs for
                     --   various sorts of users based on group membership
                   | WorkerStatusUpdate
                     (EntityID Job, JobResult)
                     -- ^ Informs the server that partial progress has been
                     --   made on a job.
                     --   TODO: Can/should we enforce the type
                     --         of incremental results?
                   | WorkerFinished
                     (EntityID Job, JobResult)
                     -- ^ Informs the server that the worker has finished,
                     --   including the 'JobResult' data
                   | WorkerSetID (EntityID WorkerProfile)
  deriving (Eq, Show, Generic)

instance A.ToJSON WorkerMessage
instance A.FromJSON WorkerMessage
