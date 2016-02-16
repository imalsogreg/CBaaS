{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Server.APIServer where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4
import Database.PostgreSQL.Simple.Types
import Servant.API
import Servant.Server
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import API
import EntityID
import Worker
import Permissions
import User
import Types
import Server.Application
import Server.Crud
import Browser
import Job

------------------------------------------------------------------------------
-- | Top-level API server implementation
serverAPI :: Server API1 AppHandler
serverAPI = serverAuth
       :<|> crudServer (Proxy :: Proxy StimulusResource)
       :<|> crudServer (Proxy :: Proxy Features)
       :<|> listWorkers
       :<|> callfun


serverAuth :: Server UserAPI AppHandler
serverAuth =
  let loginServer li = with auth $ do
        u <- loginByUsername (_liUsername li)
                             (ClearText . encodeUtf8 $ _liPassword li)
                             (_liRemember li)
        either (error "login error") return u

      registerServer ri = with auth $ do
        u <- createUser (_riUsername ri) (encodeUtf8 (_riPassword ri))
        either (error "Registration error") return u

      currentUserServer = with auth currentUser

      logoutServer = with auth logout

  in loginServer :<|> registerServer :<|> currentUserServer :<|> logoutServer


instance Crud StimulusResource where
  tableName   _ = "stimulusresource"
  tableRows _ = [RowDescription "meta"  "json" ""
                ,RowDescription "value" "json" ""
                ]


instance Crud Features where
  tableName _ = "features"
  tableRows _ = [RowDescription "meta" "json" ""
                ,RowDescription "value" "json" ""
                ]


crudServer :: forall v.Crud v => Proxy v -> Server (CrudAPI EntityID v) AppHandler
crudServer p =
  let getAll   = query_ (getAllQuery p)

      get i    = do
        rs <- query (getOneQuery p) (Only i)
        case rs of
          [r] -> return r
          _        -> error "Get failure"

      post v   = do
        rs <- query (postQuery p) v
        case rs of
          [Only r] -> return r
          _   -> error "Post failure"

      put i v  = do
        rs <- query (putQuery p) (i :. v)
        case rs of
          [Only b] -> return b
          _        -> error "Put error"

      delete i = do
        n <- query (deleteQuery p) (Only i)
        case n of
          [Only (1 :: Int)] -> return True
          _        -> error "Delete error"

  in getAll :<|> get :<|> post :<|> put :<|> delete

listWorkers :: Server (Get '[JSON] WorkerMap) AppHandler
listWorkers = do
  wrks <- liftIO . atomically . readTVar =<< gets _workers
  return (WorkerMap $ Map.map _wProfile wrks)

callfun
  :: Server (QueryParam "worker-id" WorkerID
             :> QueryParam "browser-id" BrowserID
             :> ReqBody '[JSON] Job
             :> Post '[JSON] JobID) AppHandler
callfun (Just wID) bID job = do
  wrks <- liftIO . atomically . readTVar =<< gets _workers
  liftIO (print $ Map.map _wProfile wrks)
  case Map.lookup wID wrks of
    Nothing -> liftIO (print "No match") >> pass
    Just w  -> do
      jID  <- JobID <$> liftIO nextRandom
      liftIO $ atomically $ writeTChan (_wJobQueue w) (jID, bID, job)
      return jID

