{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Server.APIServer where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Database.PostgreSQL.Simple
import Data.Maybe (listToMaybe)
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

class (ToRow v, FromRow v) => Crud v where
  getAllQuery :: Proxy v -> Query
  getQuery    :: Proxy v -> Query
  postQuery   :: Proxy v -> Query
  putQuery    :: Proxy v -> Query
  deleteQuery :: Proxy v -> Query

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
  let getAll   = lift (query_ (getAllQuery p))

      get i    = lift $ do
        rs <- query (getQuery p) (Only i)
        case rs of
          [r] -> return r
          _        -> error "Get failure"

      post v   = lift $ do
        rs <- query (postQuery p) v
        case rs of
          [Only r] -> return r
          _   -> error "Post failure"

      put i v  = lift $ do
        rs <- query (putQuery p) (i :. v)
        case rs of
          [Only b] -> return b
          _        -> error "Put error"

      delete i = lift $ do
        n <- query (deleteQuery p) (Only i)
        case n of
          [Only (1 :: Int)] -> return True
          _        -> error "Delete error"

  in getAll :<|> get :<|> post :<|> put :<|> delete
