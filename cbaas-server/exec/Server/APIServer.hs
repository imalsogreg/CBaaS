{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections   #-}

module Server.APIServer where

import Control.Concurrent.STM
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4
import qualified Database.Groundhog as GH
import Network.WebSockets
import Network.WebSockets.Snap
import Servant.API
import Servant.Server hiding (err300)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Text.Read

import API
import EntityID
import Worker
import WorkerProfile
import Permissions
import User
import Server.Application
import Server.Crud
import BrowserProfile
import Browser
import Job
import Message
import Model
import Server.Utils
import Server.WebSocketServer

------------------------------------------------------------------------------
-- | Top-level API server implementation
serverAPI :: Server API1 AppHandler
serverAPI = serverAuth
       :<|> listOnlineWorkers
       :<|> callfun
       :<|> getJobResult
       :<|> returnFun
       :<|> serveBrowserWS
       :<|> serveWorkerWS


serverAuth :: Server UserAPI AppHandler
serverAuth =
  let loginServer li = with auth $ do
        u <- loginByUsername (_liUsername li)
                             (ClearText $ encodeUtf8 $ _liPassword li)
                             (_liRemember li)
        either error return (getUserId =<< first show u)

      registerServer ri = with auth $ do
        u <- createUser (_riUsername ri) (encodeUtf8 (_riPassword ri))
        either error return (getUserId =<< first show u)

      currentUserServer = do
        mu <- with auth currentUser
        return $ fmap (either error id . getUserId) mu

      logoutServer = with auth logout

      getUserId :: AuthUser -> Either String Int
      getUserId u = do
        uid <- note "No UserId" (userId u)
        note "Malformed UserId" (readMaybe . T.unpack . unUid $ uid)

  in loginServer :<|> registerServer :<|> currentUserServer :<|> logoutServer


-- crudServer :: forall v.Crud v => Proxy v -> Server (CrudAPI (EntityID v) v) AppHandler
-- crudServer p = undefined -- TODO redo this for Groundhoge
  -- let getAll   = query_ (getAllQuery p)

  --     get i    = do
  --       rs <- query (getOneQuery p) (Only i)
  --       case rs of
  --         [r] -> return r
  --         _        -> error "Get failure"

  --     post v   = do
  --       rs <- query (postQuery p) v
  --       case rs of
  --         [Only r] -> return r
  --         _   -> error "Post failure"

  --     put i v  = do
  --       rs <- query (putQuery p) (i :. v)
  --       case rs of
  --         [Only b] -> return b
  --         _        -> error "Put error"

  --     delete i = do
  --       n <- query (deleteQuery p) (Only i)
  --       case n of
  --         [Only (1 :: Int)] -> return True
  --         _        -> error "Delete error"

  -- in getAll :<|> get :<|> post :<|> put :<|> delete

listOnlineWorkers :: Server (Get '[JSON] WorkerProfileMap) AppHandler
listOnlineWorkers = do
  wrks <- liftIO . atomically . readTVar =<< gets _workers
  return (EntityMap $ Map.map _wProfile wrks)

callfun :: Maybe WorkerProfileId
        -> Job
        -> AppHandler (EntityID Job)
callfun (Just wID :: Maybe WorkerProfileId) job = do
  wrks <- liftIO . atomically . readTVar =<< gets _workers
  case Map.lookup wID wrks of
    Nothing                  -> liftIO (print "No match") >> pass
    (Just w :: Maybe Worker) -> do
      jID  <- EntityID <$> liftIO nextRandom
      liftIO $ print "WRITING TO TCHAN"
      liftIO $ atomically $ writeTChan (_wJobQueue w) (jID, job)
      return jID


-------------------------------------------------------------------------------
-- | Handler for workers returning results
returnFun :: Maybe WorkerProfileId
          -> Maybe (EntityID Job)
          -> JobResult
          -> AppHandler ()
returnFun Nothing _ _     = err300 "Missing required parameter worker-id"
returnFun _ Nothing _     = err300 "Missing required parameter job-id"
returnFun (Just wID) (Just jobID) jr = do
  k <- runGH $ GH.insert jr
  liftIO $ putStrLn $ "INSERTION KEY: " ++ show k
  broadcastBrowsers (JobFinished jobID)
  -- resolveJob jr

broadcastBrowsers :: BrowserMessage -> AppHandler ()
broadcastBrowsers msg = do
  bs <- (liftIO . readTVarIO) =<< gets _browsers
  liftIO $ mapM_ (\b -> atomically $ writeTChan (bMessages b) msg) bs



getJobResult :: Maybe (EntityID Job) -> Handler App App JobResult
getJobResult Nothing = err300 "Missing parameter job-id"
getJobResult (Just jobID) = do
  liftIO (putStrLn $ "Get job result for " ++ show jobID)
  q <- runGH $ GH.select (JrJobField GH.==. jobID)
  case q of
    []  -> err300 $ "Found no job result at " ++ show jobID
    [r] -> return (r :: JobResult)
    _   -> err300 $ "Too many job results at " ++ show jobID



serveBrowserWS :: Server Raw AppHandler
serveBrowserWS = do
  brs <- gets _browsers
  wks <- gets _workers
  runWebSocketsSnap $ \pending -> runBrowser pending brs wks


serveWorkerWS :: Maybe WorkerName -> Maybe Text -> Maybe Type -> AppHandler ()
serveWorkerWS (Just wName) (Just fName) (Just fType) = do
  brs     <- gets _browsers
  wks     <- gets _workers
  liftIO $ print "ServeWorker: about to runWebSockets"
  runWebSocketsSnap $ \pending -> do
    print "Running"
    runWorker pending (WorkerProfile wName (fName, fType)) wks brs
serveWorkerWS Nothing _ _ = do
  liftIO $ print "No name"
  writeBS "Please give a 'worker' parameter"
serveWorkerWS _ Nothing _ = do
  liftIO $ print "No function"
  writeBS "Please give a 'function' parameter"
serveWorkerWS _ _ Nothing = do
  liftIO $ print "No type"
  writeBS "Please give a 'type' parameter"
