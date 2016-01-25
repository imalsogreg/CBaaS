{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Job where

import Control.Monad (mzero)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text
import Data.UUID

import Model

newtype JobID = JobID { _unJobID :: UUID }
  deriving (Eq, Show, Ord)

instance ToJSON JobID where
  toJSON (JobID uu) = String $ toText uu

instance FromJSON JobID where
  parseJSON (String s) = maybe mzero (return . JobID) (fromText s)
  parseJSON _          = mzero

makeLenses ''JobID

data Job = Job
  { _jID      :: JobID
  , _jFunName :: Text
  , _jArgs    :: [Model.Val]
  } deriving (Eq, Show)

makeLenses ''Job

instance ToJSON Job where
  toJSON j = object ["id"       .= (j^.jID)
                    ,"function" .= (j^.jFunName)
                    ,"args"     .= (j^.jArgs)]

instance FromJSON Job where
  parseJSON (Object o) = Job
    <$> o .: "id"
    <*> o .: "function"
    <*> o .: "args"
  parseJSON _ = undefined
