{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.WebSocketServer (
  fanoutResults,
  runBrowser,
  runWorker) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad.State
import qualified Data.Aeson as A
import Data.ByteString.Char8 (ByteString)
import Data.Map
import qualified Data.Map as Map
import Data.UUID.V4
import System.IO
import qualified Network.WebSockets as WS
import URI.ByteString

import EntityID
import Job
import Message
import qualified Model as Model
import Worker
import WorkerProfile
import BrowserProfile
import Browser


------------------------------------------------------------------------------
-- | Watch results channel and send results to the browser that
--   initially requested the job
fanoutResults :: TChan (EntityID Job, Maybe (BrowserProfileId), JobResult)
              -> TVar BrowserMap -> IO ()
fanoutResults results browsers = forever $ do
  (browserMatch, res) <- atomically $ do
    (wrk,br,res) <- readTChan results
    brs <- readTVar  browsers
    return (flip Map.lookup brs =<< br, res)
  case browserMatch of
    Nothing -> return ()
    Just b  -> WS.sendTextData (bConn b) (A.encode (JobFinished (jrJob res, res)))


runBrowser :: WS.PendingConnection
           -> TVar BrowserMap
           -> TVar (Map.Map (EntityID WorkerProfile) Worker)
           -> IO ()
runBrowser pending browsers workers = do
  lastWorkers <- newTVarIO Map.empty
  conn <- WS.acceptRequest pending
  i    <- EntityID <$> nextRandom
  WS.sendTextData conn (A.encode $ SetBrowserID i)
  res  <- newTChanIO
  atomically $ modifyTVar browsers
    (Map.insert i (Browser i conn res))

  flip finally (disconnect i) $ do
    _ <- forkIO (process conn lastWorkers workers)
    _ <- forkIO (runChannel conn res)
    listen conn res
    print "Finished listening."
  where

    disconnect i =
      atomically $ modifyTVar browsers (Map.delete i)

    runChannel conn resultsChan = forever $ do
      atomically (readTChan resultsChan) >>= \r -> do
        WS.sendTextData conn (A.encode r)

    listen conn resultsChan = forever $ do
      WS.receiveData conn >>= \(d :: ByteString) ->
        print "Really wasn't expecting messages here."

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


------------------------------------------------------------------------------
-- | Setup a worker entry and fork a thread for talking with a worker client
--   process
runWorker :: WS.PendingConnection
          -> WorkerProfile
          -- ^ Worker setup info
          -> TVar (Map (EntityID WorkerProfile) Worker)
          -- ^ Worker map - for self-inserting and deleting
          -> TVar BrowserMap
          -> TChan (EntityID Job, Maybe (BrowserProfileId), JobResult)
          -- ^ Completion write channel
          -> IO ()
runWorker pending wp workers browsers resultsChan = do
  conn     <- WS.acceptRequest pending
  i :: EntityID WorkerProfile <- EntityID <$> nextRandom
  jobsChan <- atomically newTChan
  let worker = Worker wp i conn jobsChan
  atomically $ modifyTVar workers (Map.insert (_wID worker) worker)
  _ <- forkIO $ talk conn jobsChan
  flip finally (disconnect i) $ forever $ do
    m <- WS.receiveData conn
    case (A.decode m :: Maybe WorkerMessage) of
      Just (WorkerFinished (_,b,r)) -> do
        print "WORKER FINISHED"
        atomically $ writeTChan resultsChan (jrJob r, b, r)
      Just (WorkerStatusUpdate (j,b,r)) ->
        atomically $ writeTChan resultsChan (j,b,r)
      Just (JobRequested _) -> do
        print "Received 'JobRequested' message in an unexpected place"
      Nothing -> error "Message decoding error"
  where
    disconnect i = atomically $ modifyTVar workers
                              $ Map.delete i

    talk :: WS.Connection
         -> TChan (EntityID Job, Maybe (BrowserProfileId), Job)
         -> IO ()
    talk conn jobsChan = do
        (jID, brID, j) <- atomically (readTChan jobsChan)
        WS.sendTextData conn (A.encode $ JobRequested (jID, brID, j))
        talk conn jobsChan

    listenTillDone :: WS.Connection -> IO (EntityID Job, Maybe (BrowserProfileId), JobResult)
    listenTillDone conn = do
        m <- WS.receiveData conn
        print $ "In listenTillDone, got: " ++ show m
        case A.decode m of
          Just (WorkerFinished (_,b,r)) ->
            return (jrJob r, b, r)
          Just (WorkerStatusUpdate (j,b,r)) -> do
            atomically $ writeTChan resultsChan (j,b,r)
            listenTillDone conn
