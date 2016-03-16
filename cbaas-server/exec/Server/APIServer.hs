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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Control.Monad.Trans.Class
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.UUID.V4
import Network.WebSockets
import Network.WebSockets.Snap
import Servant.API
import Servant.Server
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
import Server.WebSocketServer

------------------------------------------------------------------------------
-- | Top-level API server implementation
serverAPI :: Server API1 AppHandler
serverAPI = serverAuth
       :<|> listOnlineWorkers
       :<|> callfun
       :<|> serveBrowserWS
       :<|> serveWorkerWS


serverAuth :: Server UserAPI AppHandler
serverAuth =
  let loginServer li = with auth $ do
        u <- loginByUsername (_liUsername li)
                             (encodeUtf8 $ _liPassword li)
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

-- callfun
--   :: Server (QueryParam "worker-id" WorkerProfileId
--              :> QueryParam "browser-id" BrowserProfileId
--              :> ReqBody '[JSON] Job
--              :> Post '[JSON] (EntityID Job)) AppHandler
callfun :: (Maybe WorkerProfileId) 
        -> (Maybe BrowserProfileId) 
        -> Job 
        -> AppHandler (EntityID Job)
callfun (Just wID :: Maybe WorkerProfileId) bID job = do
  wrks :: Map.Map (EntityID WorkerProfile) Worker <-
    liftIO . atomically . readTVar =<< gets _workers
  case Map.lookup wID wrks of
    Nothing                  -> liftIO (print "No match") >> pass
    (Just w :: Maybe Worker) -> do
      jID  <- EntityID <$> liftIO nextRandom
      liftIO $ atomically $ writeTChan (_wJobQueue w) (jID, bID, job)
      return jID


-- resolveFunction :: Maybe FunctionName
--                 -> Maybe Type
--                 -> [Tag]
--                 -> Bool
--                 -> AppHandler RemoteFunction
-- resolveFunction Nothing _ _ _ _ = err300 "Function name is mandatory"
-- resolveFunction (Just fName) fType necessaryTags strictlyOne = do
--   rs     <- select funQueryConditions
--   tags   <- select (FunctionTagFunctionField ==. fName)
  
--   where funQueryConditions Nothing  = FunctionNameField ==. fName
--         funQueryConditions (Just t) = FunctionNameField ==. fName &&.
--                                       FunctionTypeField ==. t


serveBrowserWS :: Server Raw AppHandler
serveBrowserWS = do
  brs <- gets _browsers
  wks <- gets _workers
  runWebSocketsSnap $ \pending -> runBrowser pending brs wks

serveWorkerWS :: Maybe WorkerName -> Maybe Text -> AppHandler ()
serveWorkerWS (Just wName) (Just fName) = do
  brs     <- gets _browsers
  wks     <- gets _workers
  results <- gets _rqueue
  liftIO $ print "ServeWorker: about to runWebSockets"
  runWebSocketsSnap $ \pending -> do
    print "Running"
    runWorker pending (WorkerProfile wName fName) wks brs results
serveWorkerWS Nothing _ = do
  liftIO $ print "No name"
  writeBS "Please give a worker-name parameter"
serveWorkerWS _ Nothing = do
  liftIO $ print "No function"
  writeBS "Please give a function-name parameter"