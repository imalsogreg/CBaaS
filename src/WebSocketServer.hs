{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer where


import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad.State
import Data.Bifunctor (first)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.List
import Data.Map
import qualified Data.Map as Map
import Snap.Snaplet.PostgresqlSimple
import System.IO
import qualified Network.WebSockets as WS
import URI.ByteString

import Job
import qualified Model as Model
import Worker

launchWebsocketServer :: Postgres
                   -- ^ SQL connection
                   -> TVar (Map WorkerID Worker)
                   -- ^ All registered workers
                   -> TChan (WorkerID, JobID, Model.Val)
                   -- ^ Read-end of a work queue
                   -> TChan (WorkerID, JobID, Model.Val)
                   -- ^ Read-end of a job completion queue
                   -> IO ()
launchWebsocketServer pg workers jobs results = do
  resultsChan <- newTChanIO
  WS.runServer "0.0.0.0" 9160 $ \pending -> do
    let uri = parseRelativeRef strictURIParserOptions (reqPath pending)

    case rrPath <$> uri of
      Right "/worker" ->
        case parseWorkerProfile =<< first show (fmap rrQuery uri) of
          Left e -> putStrLn e
          Right wp -> do
            worker <- initializeWorker pending wp resultsChan
            atomically (modifyTVar workers (Map.insert (_wID worker) worker))
            print "Saw a wp"
            putStrLn "Launching Worker WS"
            hFlush stdout
            -- WS.sendTextData c ("Hello websocket!" :: ByteString)
            -- r <- WS.receiveData c
            -- print (unpack r)
            -- myChan <- atomically $ dupTChan jobs
            return ()
      Right "/browser" -> do
        putStrLn "Launching Browser WS"
        hFlush stdout
        return ()
      _ -> do
        putStrLn $ "Bad request path: " ++ unpack (reqPath pending)
        hFlush stdout

  where reqPath = WS.requestPath . WS.pendingRequest

data Message = WorkerJoined Worker
             | WorkerLeft   WorkerID
             | RunJob       JobID
