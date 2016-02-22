{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import EntityID
import Job
import Message
import qualified Model as Model
import Worker
import Browser

launchWebsocketServer :: Postgres
                   -- ^ SQL connection
                   -> TVar (Map.Map (EntityID WorkerProfile) Worker)
                   -- ^ All registered workers
                   -> TVar BrowserMap
                   -> TChan (EntityID Worker, EntityID Job, Model.Val)
                   -- ^ Read-end of a work queue
                   -> TChan (EntityID Worker, EntityID Job, Model.Val)
                   -- ^ Read-end of a job completion queue
                   -> IO ()
launchWebsocketServer pg workers browsers jobs results = do
  resultsChan <- newTChanIO
  _ <- forkIO (fanoutResults resultsChan browsers)
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

fanoutResults :: TChan (EntityID Job, Maybe (EntityID Browser), JobResult)
              -> TVar BrowserMap -> IO ()
fanoutResults results browsers = forever $ do
  (browserMatch, res) <- atomically $ do
    (wrk,br,res) <- readTChan results
    EntityMap brs <- readTVar  browsers
    return (flip Map.lookup brs =<< br, res)
  case browserMatch of
    Nothing -> return ()
    Just b  -> WS.sendTextData (bConn b) (A.encode (JobFinished (jrJob res, res)))

type WorkerMap = Map.Map (EntityID WorkerProfile) Worker

runBrowser :: WS.PendingConnection
           -> TVar BrowserMap
           -> TVar (Map.Map (EntityID WorkerProfile) Worker)
           -> IO ()
runBrowser pending browsers workers = do
  print "Run browser"
  lastWorkers <- newTVarIO $ Map.empty
  conn <- WS.acceptRequest pending
  i    <- EntityID <$> nextRandom
  WS.sendTextData conn (A.encode $ SetBrowserID i)
  res  <- newTChanIO
  let browser = Browser i conn res
  atomically $ modifyTVar browsers (EntityMap . Map.insert i browser . unEntityMap)
  flip finally (disconnect i) $ do
    _ <- forkIO (process conn lastWorkers workers)
    listen conn res
  where
    disconnect i =
      atomically $ modifyTVar browsers (EntityMap . Map.delete i . unEntityMap)
    process :: WS.Connection
            -> TVar (Map (EntityID WorkerProfile) Worker)
            -> TVar (Map (EntityID WorkerProfile) Worker)
            -> IO ()
    process conn wsVar wsVar' = do
      (newWorkers, leftWorkers) <- atomically $ do
        ws' <- readTVar wsVar'
        ws  <- readTVar wsVar
        let newWorkers  = Map.difference ws' ws
            leftWorkers = Map.difference ws ws'
        when (Map.null newWorkers && Map.null leftWorkers) retry
        writeTVar wsVar ws'
        return (newWorkers , leftWorkers)
      forM_ (Map.toList newWorkers) $ \(wi, w) ->
        WS.sendTextData conn (A.encode (WorkerJoined wi (_wProfile w)))
      forM_ (Map.toList leftWorkers) $ \(wi, _) ->
        WS.sendTextData conn (A.encode (WorkerLeft wi))
      process conn wsVar wsVar'
    listen conn resultsChan = do
      _ <- forkIO $ forever $ atomically (readTChan resultsChan) >>= \r ->
        WS.sendTextData conn (A.encode r)
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close n b) -> print "Browser disconnect msg"
        x -> print x >> listen conn resultsChan


------------------------------------------------------------------------------
-- Setup a worker and fork a thread for talking with a worker client process
runWorker :: WS.PendingConnection
          -> WorkerProfile
          -- ^ Worker setup info
          -> TVar (Map (EntityID WorkerProfile) Worker)
          -- ^ Worker map - for self-inserting and deleting
          -> TVar BrowserMap
          -> TChan (EntityID Job, Maybe (EntityID Browser), JobResult)
          -- ^ Completion write channel
          -> IO ()
runWorker pending wp workers browsers resultsChan = do
  conn     <- WS.acceptRequest pending
  i :: EntityID WorkerProfile <- EntityID <$> nextRandom
  jobsChan <- atomically newTChan
  let worker = Worker wp i conn jobsChan
  atomically $ modifyTVar workers (Map.insert (_wID worker) worker)
  flip finally (disconnect i) $ talk conn jobsChan
  where
    disconnect i = atomically $ modifyTVar workers
                              $ Map.delete i

    talk :: WS.Connection
         -> TChan (EntityID Job, Maybe (EntityID Browser), Job)
         -> IO ()
    talk conn jobsChan = do
      (jID, brID, j) <- atomically (readTChan jobsChan)
      WS.sendTextData conn (A.encode $ JobRequested (jID, brID, j))
      print "listenTillDone"
      (jID', brID', jr) <- listenTillDone conn
      print "WriteTChan after listening"
      atomically (writeTChan resultsChan (jID', brID', jr))
      talk conn jobsChan

    listenTillDone :: WS.Connection
                   -> IO (EntityID Job, Maybe (EntityID Browser), JobResult)
    listenTillDone conn = do
      msg <- WS.receive conn
      case msg of
        WS.ControlMessage (WS.Close n b) -> throw (WS.CloseRequest n b)
        WS.DataMessage m -> do
          let contents = case m of
                WS.Text c   -> c
                WS.Binary c -> c
          case A.decode contents of
            Just (WorkerFinished (_,b,r)) ->
              return (jrJob r, b, r)
            Just (WorkerStatusUpdate (j,b,r)) -> do
              atomically $ writeTChan resultsChan (j,b,r)
              listenTillDone conn
