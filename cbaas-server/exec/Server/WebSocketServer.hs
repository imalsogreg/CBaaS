{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.WebSocketServer (
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
import Data.Text (Text)
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

    runChannel conn resultsChan = forever $
      atomically (readTChan resultsChan) >>= \r ->
        WS.sendTextData conn (A.encode r)

    listen conn resultsChan = forever $
      WS.receiveData conn >>= \case
        ("ping" :: ByteString) -> return ()
        _                      -> print "Really wasn't expecting messages here."

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
          -> IO ()
runWorker pending wp workers browsers = do
  conn     <- WS.acceptRequest pending
  i :: EntityID WorkerProfile <- EntityID <$> nextRandom
  jobsChan <- atomically newTChan
  let worker = Worker wp i conn jobsChan
  atomically $ modifyTVar workers (Map.insert (_wID worker) worker)

  _ <- forkIO $ do
    WS.sendTextData conn (A.encode $ WorkerSetID (_wID worker))
    sendJob conn jobsChan

  flip finally (disconnect i) $ forever $ do
    m :: Text <- WS.receiveData conn
    return ()  -- We don't expect to receive any messages back from the worker
               -- Worker's result comes back over http
     where

    sendJob :: WS.Connection -> TChan (EntityID Job, Job) -> IO ()
    sendJob conn jobsChan = do
        print "**Worker waiting**"
        (jID, j) <- atomically (readTChan jobsChan)
        print "**Worker sending**"
        WS.sendTextData conn (A.encode $ JobRequested (jID, j))
        print "**Worker sent**"
        sendJob conn jobsChan

    disconnect i = atomically $ modifyTVar workers
                              $ Map.delete i

