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
  forkIO (fanoutResults resultsChan browsers)
  WS.runServer "0.0.0.0" 9160 $ \pending -> do
    let uri = parseRelativeRef strictURIParserOptions (reqPath pending)

    case rrPath <$> uri of
      Right "/worker" ->
        case parseWorkerProfile =<< first show (fmap rrQuery uri) of
          Left e -> putStrLn e
          Right wp -> do
            () <- runWorker pending wp workers browsers resultsChan
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

fanoutResults :: TChan (JobID, Maybe BrowserID, JobResult) -> TVar (Map BrowserID Browser) -> IO ()
fanoutResults results browsers = forever $ do
  (browserMatch, res) <- atomically $ do
    (wrk,br,res) <- readTChan results
    brs <- readTVar  browsers
    return (flip Map.lookup brs =<< br, res)
  print "BrowserMatch:"
  print (bID <$> browserMatch)
  case browserMatch of
    Nothing -> return ()
    Just b  -> WS.sendTextData (bConn b) (A.encode (JobFinished (jrJob res, res)))

data BrowserMessage = WorkerJoined WorkerID WorkerProfile
                    | WorkerLeft   WorkerID
                    | JobFinished  (JobID, JobResult)
                    | SetBrowserID BrowserID
  deriving (Eq, Generic)

instance A.ToJSON BrowserMessage where
instance A.FromJSON BrowserMessage where

data WorkerMessage = JobRequested   (JobID, Maybe BrowserID, Job)
                   | WorkerFinished (JobID, Maybe BrowserID, JobResult)
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
  WS.sendTextData conn (A.encode $ SetBrowserID i)
  res  <- newTChanIO
  let browser = Browser i conn res
  atomically $ modifyTVar browsers (Map.insert i browser)
  flip finally (disconnect i) $ do
    _ <- forkIO (process conn lastWorkers workers)
    listen conn res
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
    listen conn resultsChan = do
      print "Listen"
      _ <- forkIO $ forever $ atomically (readTChan resultsChan) >>= \r ->
        WS.sendTextData conn (A.encode r)
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close n b) -> print "Browser disconnect"
        x -> print x >> listen conn resultsChan


------------------------------------------------------------------------------
-- Setup a worker and fork a thread for talking with a worker client process
runWorker :: WS.PendingConnection
          -> WorkerProfile
          -- ^ Worker setup info
          -> TVar (Map.Map WorkerID Worker)
          -- ^ Worker map - for self-inserting and deleting
          -> TVar (Map.Map BrowserID Browser)
          -> TChan (JobID, Maybe BrowserID, JobResult)
          -- ^ Completion write channel
          -> IO ()
runWorker pending wp workers browsers resultsChan = do
  conn     <- WS.acceptRequest pending
  i        <- WorkerID <$> nextRandom
  jobsChan <- atomically newTChan
  let worker = Worker wp i conn jobsChan
  atomically $ modifyTVar workers (Map.insert (_wID worker) worker)
  flip finally (disconnect i) $ do
        _ <- forkIO $ forever $ do
          (jID, brID, j) <- atomically $ readTChan jobsChan
          WS.sendTextData conn (A.encode $ JobRequested (jID, brID, j))
        listen conn resultsChan
  where
    disconnect i = do
        print "Disconnect"
        atomically $ modifyTVar workers $ Map.delete i
    listen :: WS.Connection -> TChan (JobID, Maybe BrowserID, JobResult) -> IO ()
    listen conn resultsChan = do
      print "Listen"
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close n b) -> throw (WS.CloseRequest n b)
        WS.DataMessage msg -> do
          let m = case msg of
                WS.Text   bs -> bs
                WS.Binary bs -> bs
          case A.decode m of
            Just (WorkerFinished (_, b, r)) -> do
              --brws <- readTVarIO browsers
              --case Map.lookup (_jrJob $ Browser jr) brws of
              atomically (writeTChan resultsChan (jrJob r,b,r))
            Nothing -> print "Error decoding result"
          listen conn resultsChan

-- data WorkerMessage = JobRequested   (JobID, Maybe BrowserID, Job)
--                    | WorkerFinished (JobID, Maybe BrowserID, JobResult)
--   deriving (Eq, Generic)

