{-# LANGUAGE DeriveGeneric #-}

module Message where

------------------------------------------------------------------------------
import qualified Data.Aeson as A
import           GHC.Generics
------------------------------------------------------------------------------
import           Browser
import           Job
import           Worker

------------------------------------------------------------------------------
-- | 'BrowserMessage's go between browser and CBaaS server
data BrowserMessage = WorkerJoined WorkerID WorkerProfile
                      -- ^ Informing the browser that a worker has joined
                    | WorkerLeft   WorkerID
                      -- ^ Informing the browser that a worker has left
                    | JobFinished  (JobID, JobResult)
                      -- ^ Informing the browser of job completion - usually
                      --   for jobs that this browser requested
                    | JobStatusUpdate (JobID, JobResult)
                      -- ^ Informing the browser of a status-update on a
                      --   job
                    | SetBrowserID BrowserID
                      -- ^ Inform the browser of its ID number
                      --   TODO: should this be asynchronous information?
                      --   TODO: Does it need to be validated or enforced
                      --         somehow?
  deriving (Eq, Show, Generic)

instance A.ToJSON BrowserMessage
instance A.FromJSON BrowserMessage

------------------------------------------------------------------------------
-- | 'WorkerMessage's go between CBaaS server and online workers
data WorkerMessage = JobRequested   (JobID, Maybe BrowserID, Job)
                     -- ^ Informs a worker that a user has requested a job.
                     --   CBaaS server is
                     --   responsible for ensuring that job requests are
                     --   filtered by function name, argument type,
                     --   and the worker's willingness to do jobs for
                     --   various sorts of users based on group membership
                   | WorkerStatusUpdate (JobID, Maybe BrowserID, JobResult)
                     -- ^ Informs the server that partial progress has been
                     --   made on a job.
                     --   TODO: Can/should we enforce the type
                     --         of incremental results?
                   | WorkerFinished (JobID, Maybe BrowserID, JobResult)
                     -- ^ Informs the server that the worker has finished,
                     --   including the 'JobResult' data
  deriving (Eq, Show, Generic)

instance A.ToJSON WorkerMessage
instance A.FromJSON WorkerMessage

