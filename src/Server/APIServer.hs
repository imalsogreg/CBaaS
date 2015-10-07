{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes     #-}

module Server.APIServer where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Database.PostgreSQL.Simple
import Data.Proxy
--import Database.PostgreSQL.Simple.SqlQQ
import Servant.API
import Servant.Server
import Snap.Snaplet.PostgresqlSimple
import API
import Types
import Server.Application

------------------------------------------------------------------------------
-- | Top-level API server implementation
serverAPI :: Server API1 AppHandler
serverAPI = serverAuth
       :<|> crudServer (Proxy :: Proxy StimulusResource)
       :<|> crudServer (Proxy :: Proxy Features)

serverAuth :: Server UserAPI AppHandler
serverAuth = undefined

instance Crud StimulusResource where
  getAllQuery _ = "SELECT * FROM stimulusresource"
  getQuery    _ = "SELECT * FROM stimulusresource where id=(?)"
  postQuery   _ = "INSERT INTO stimulusresource VALUES ?"
  putQuery    _ = "UPDATE stimulusresource (id, url, mime) with values (?,?,?)"
  deleteQuery _ = "DELETE FROM stimilusresource WHERE id=(?)"

instance Crud Features where
  getAllQuery _ = "SELECT * FROM features"
  getQuery    _ = "SELECT * FROM features where id=(?)"
  postQuery   _ = "INSERT INTO features VALUES ?"
  putQuery    _ = "UPDATE features (id, meta, data) with values (?,?,?)"
  deleteQuery _ = "DELETE FROM features WHERE id=(?)"

crudServer :: forall v.Crud v => Proxy v -> Server (CrudAPI EntityID v) AppHandler
crudServer p =
  let getAll   = query_ (getAllQuery p) :: AppHandler [v]
      get i    = undefined -- query (getQuery p) i
      post v   = undefined -- query (postQuery p) v
      put i v  = undefined -- query (putQuery p) (i, v)
      delete i = undefined -- query (deleteQuery p) i
  in lift getAll :<|> lift get :<|> lift post :<|> lift put :<|> lift delete

-- class CrudPair i v where
--   indexOf     :: v -> i
--   getAllQuery :: FromRow v =>      Server (Get    '[JSON] [(i,v)])   AppHandler
--   getQuery    :: FromRow v => i -> Server (Get    '[JSON] (Maybe v)) AppHandler
--   postQuery   :: ToRow v =>   v -> Server (Post   '[JSON] (Maybe i)) AppHandler
--   putQuery    :: ToRow v =>   v -> Server (Put    '[JSON]  Bool)     AppHandler
--   deleteQuery ::              i -> Server (Delete '[JSON]  Bool)     AppHandler

-- class CrudPair i v where
--   indexOf     :: v -> i
--   getAllQuery :: (HasPostgres m, FromRow v) =>      m [(i,v)]
--   getQuery    :: (HasPostgres m, FromRow v) => i -> m (Maybe v)
--   postQuery   :: (HasPostgres m, ToRow   v) => v -> m (Maybe i)
--   putQuery    :: (HasPostgres m, ToRow   v) => v -> m Bool
--   deleteQuery :: (HasPostgres m)            => i -> m Bool

class Crud v where
  getAllQuery :: Proxy v -> Query
  getQuery    :: Proxy v -> Query
  postQuery   :: Proxy v -> Query
  putQuery    :: Proxy v -> Query
  deleteQuery :: Proxy v -> Query


-- crudGetOne :: (MonadIO m, (CrudPair i v)) => i ->      m (Maybe v)
-- crudGetOne i = undefined
-- crudGetAll :: (MonadIO m, (CrudPair i v)) =>           m [(i,v)]
-- crudGetAll = undefined
-- crudPost   :: (MonadIO m, (CrudPair i v)) => v ->      m (Maybe i)
-- crudPost = undefined
-- crudPut    :: (MonadIO m, (CrudPair i v))   => i -> v -> m Bool
-- crudPut = undefined
-- crudDelete :: (MonadIO m)                        => i ->      m Bool
-- crudDelete = undefined


serverCrud :: (ToRow (i :. v)) => Server (CrudAPI i v) AppHandler
serverCrud = undefined
-- serverCrud = serverGetAll
--         :<|> serverGet
--         :<|> serverPost
--         :<|> serverPut
--         :<|> serverDelete
--   where serverGetAll = with db $ query "SELECT * FROM"
