module Job where

import Data.UUID

import Model

newtype JobID = JobId { unJobID :: UUID }

data Job = Job -- TODO
