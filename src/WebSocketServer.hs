{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module WebSocketServer where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Monad.State
import qualified Data.Aeson as A
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.List
import Data.Map
import qualified Data.Map as Map
import Data.UUID.V4
import GHC.Generics
import Snap.Snaplet.PostgresqlSimple
import System.IO
import qualified Network.WebSockets as WS
import URI.ByteString

import Job
import qualified Model as Model
import Worker
import Browser

launchWebsocketServer :: Postgres
                   -- ^ SQL connection
                   -> TVar (Map WorkerID Worker)
                   -- ^ All registered workers
                   -> TVar (Map BrowserID Browser)
                   -> TChan (WorkerID, JobID, Model.Val)
                   -- ^ Read-end of a work queue
                   -> TChan (WorkerID, JobID, Model.Val)
                   -- ^ Read-end of a job completion queue
                   -> IO ()
launchWebsocketServer pg workers browsers jobs results = do
  resultsChan <- newTChanIO
  WS.runServer "0.0.0.0" 9160 $ \pending -> do
    let uri = parseRelativeRef strictURIParserOptions (reqPath pending)

    case rrPath <$> uri of
      Right "/worker" ->
        case parseWorkerProfile =<< first show (fmap rrQuery uri) of
          Left e -> putStrLn e
          Right wp -> do
            () <- runWorker pending wp workers resultsChan
            print "Saw a wp"
            putStrLn "Running Worker WS"
            hFlush stdout
            return ()
      Right "/browser" -> do
        runBrowser pending browsers workers
        putStrLn "Running Browser WS"
        hFlush stdout
        return ()
      _ -> do
        putStrLn $ "Bad request path: " ++ unpack (reqPath pending)
        hFlush stdout

  where reqPath = WS.requestPath . WS.pendingRequest

data BrowserMessage = WorkerJoined WorkerID WorkerProfile
                    | WorkerLeft   WorkerID
                    | JobFinished  (JobID, JobResult)
  deriving (Eq, Generic)

instance A.ToJSON BrowserMessage where
instance A.FromJSON BrowserMessage where

data WorkerMessage = JobRequested Job
  deriving (Eq, Generic)

instance A.ToJSON WorkerMessage
instance A.FromJSON WorkerMessage

runBrowser :: WS.PendingConnection
           -> TVar (Map.Map BrowserID Browser)
           -> TVar (Map.Map WorkerID  Worker)
           -> IO ()
runBrowser pending browsers workers = do
  print "Run browser"
  lastWorkers <- newTVarIO Map.empty
  conn <- WS.acceptRequest pending
  i    <- BrowserID <$> nextRandom
  res  <- newTChanIO
  let browser = Browser i conn res
  atomically $ modifyTVar browsers (Map.insert i browser)
  flip finally (disconnect i) $ do
    _ <- forkIO (process conn lastWorkers workers)
    listen conn
  where
    disconnect i = do
      print "Browser disconnect"
      atomically $ modifyTVar browsers (Map.delete i)
    process conn wsVar wsVar' = do
      (newWorkers, leftWorkers) <- atomically $ do
        ws' <- readTVar wsVar'
        ws  <- readTVar wsVar
        let newWorkers  = Map.difference ws' ws
            leftWorkers = Map.difference ws ws'
        when (Map.null newWorkers && Map.null leftWorkers) retry
        writeTVar wsVar ws'
        return (newWorkers :: Map.Map WorkerID Worker, leftWorkers)
      forM_ (Map.toList newWorkers) $ \(wi, w) -> do
        print "SENDING NEW"
        WS.sendTextData conn (A.encode (WorkerJoined wi (_wProfile w)))
      forM_ (Map.toList leftWorkers) $ \(wi, _) -> do
        print "SENDING LEFT"
        WS.sendTextData conn (A.encode (WorkerLeft wi))
      process conn wsVar wsVar'
    listen conn = do
      print "Listen"
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close n b) -> print "Browser disconnect"
        x -> print x >> listen conn


------------------------------------------------------------------------------
-- Setup a worker and fork a thread for talking with a worker client process
runWorker :: WS.PendingConnection
          -> WorkerProfile
          -- ^ Worker setup info
          -> TVar (Map.Map WorkerID Worker)
          -- ^ Worker map - for self-inserting and deleting
          -> TChan (WorkerID, JobID, Model.Val)
          -- ^ Completion write channel
          -> IO ()
runWorker pending wp workers resultsChan = do
  conn     <- WS.acceptRequest pending
  i        <- WorkerID <$> nextRandom
  jobsChan <- atomically newTChan
  let worker = Worker wp i conn jobsChan
  atomically $ modifyTVar workers (Map.insert (_wID worker) worker)
  flip finally (disconnect i) $ do
        _ <- forkIO $ forever $ do
          job <- atomically $ readTChan jobsChan
          WS.sendTextData conn (A.encode job)
        listen conn
  where
    disconnect i = do
        print "Disconnect"
        atomically $ modifyTVar workers $ Map.delete i
    listen conn = do
      print "Listen"
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close n b) -> throw (WS.CloseRequest n b)
        x -> print x >> listen conn
