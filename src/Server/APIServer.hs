{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}

module Server.APIServer where

import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Servant.API
import Servant.Server
import API
import Types
import Server.Application

------------------------------------------------------------------------------
-- | Top-level API server implementation
serverAPI :: Server API1 AppHandler
serverAPI = serverAuth :<|> serverCrud :<|> serverCrud

serverAuth :: Server UserAPI AppHandler
serverAuth = undefined

instance CrudPair EntityID StimulusResource where
  type (CrudRow EntityID StimulusResource) = (EntityID, StimulusResource)
  crudInd = fst
  crudVal = snd

-- class CrudPair i v where
--   type CrudRow i v :: *
--   crudInd :: CrudRow i v -> i
--   crudVal :: CrudRow i v -> v

crudGetOne :: (MonadIO m, FromRow (CrudRow i v)) => i ->      m (Maybe v)
crudGetOne i = undefined
crudGetAll :: (MonadIO m, FromRow (CrudRow i v)) =>           m [(i,v)]
crudGetAll = undefined
crudPost   :: (MonadIO m, FromRow (CrudRow i v)) => v ->      m (Maybe i)
crudPost = undefined
crudPut    :: (MonadIO m, ToRow (CrudRow i v))   => i -> v -> m Bool
crudPut = undefined
crudDelete :: (MonadIO m)                        => i ->      m Bool
crudDelete = undefined


serverCrud :: (ToRow (i :. v)) => Server (CrudAPI i v) AppHandler
serverCrud = serverGetAll
        :<|> serverGet
        :<|> serverPost
        :<|> serverPut
        :<|> serverDelete
  where serverGetAll = 
