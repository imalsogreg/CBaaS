{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language TypeFamilies #-}

module WorkerProfile where

-- import Control.Concurrent
-- import Control.Concurrent.STM
-- import Control.Exception
-- import Control.Monad (forever, mzero)
-- import Data.Aeson
-- import qualified Data.Aeson as A
-- import qualified Data.Map as Map
-- import Data.Text hiding (map, filter)
-- import qualified Data.ByteString.Char8 as BS
-- import Data.Text.Encoding
-- import Data.UUID.Types
-- import qualified Data.UUID.Types as UUID
-- import GHC.Generics
-- import Servant.API
-- import URI.ByteString  hiding (Query) 
-- import Web.HttpApiData  hiding (Query) 
-- #ifndef __GHCJS__
-- import Database.Groundhog  hiding (Query) 
-- import Database.Groundhog.Generic  hiding (Query) 
-- #endif
-- import Database.Groundhog.Core  hiding (Query) 
-- import Database.Groundhog.TH  hiding (Query) 
-- import Text.Read (readMaybe)

-- import BrowserProfile hiding (Query) 
-- import EntityID hiding (Query) 
-- import Job hiding (Query) 
-- import Model hiding (Query) 
-- import RemoteFunction hiding (Query) 
-- import Utils hiding (Query) 

-- type WorkerProfileMap = EntityMap WorkerProfile
-- type WorkerProfileId  = EntityID  WorkerProfile


-- data WorkerProfile = WorkerProfile
--   { wpName     :: WorkerName
--   , wpFunction :: (Text, Type)
--   } deriving (Eq, Ord, Show)

-- instance A.ToJSON WorkerProfile where
--   toJSON (WorkerProfile (WorkerName n) (f,t)) =
--     A.object ["name" .= n
--              ,"function" .= f
--              ,"type" .= t
--              ]

-- instance A.FromJSON WorkerProfile where
--   parseJSON (A.Object o) = do
--     n <- o .: "name"
--     f <- o .: "function"
--     t <- o .: "type"
--     return $ WorkerProfile n (f,t)

-- instance FromHttpApiData WorkerName where
--   parseUrlPiece = Right . WorkerName

-- data WorkerName = WorkerName { unWorkerName :: Text }
--   deriving (Eq, Ord, Show, Read, GHC.Generics.Generic)

-- instance A.ToJSON WorkerName where
--   toJSON (WorkerName n) = A.String n

-- instance A.FromJSON WorkerName where
--   parseJSON (A.String n) = return $ WorkerName n
--   parseJSON _ = mzero


-- parseWorkerProfile :: Query -> Either String WorkerProfile
-- parseWorkerProfile q = do
--   nm  <- note "No WorkerProfile name"     (lookup "name" ps)
--   fn  <- note "No WorkerProfile function" (lookup "function" ps)
--   fty <- note "No WorkerProfile type" (lookup "type" ps) >>= (note "No parse" . readMaybe . BS.unpack)
--   return $ WorkerProfile (WorkerName $ decodeUtf8 nm) (decodeUtf8 fn, fty)
--   where ps = queryPairs q

-- #ifndef __GHCJS__
-- -- TODO custom WorkerName instance to avoid showing/reading constructors
-- mkPersist ghCodeGen [groundhog|
--   - primitive: WorkerName
--     converter: showReadConverter
--   - entity: WorkerProfile
-- |]
-- #endif
