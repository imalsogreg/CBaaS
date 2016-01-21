{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer where


import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.ByteString.Char8 (unpack)
import Data.List
import Data.Map
import Snap.Snaplet.PostgresqlSimple
import System.IO
import qualified Network.WebSockets as WS

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
  WS.runServer "0.0.0.0" 9160 $ \pending -> do
    c <- WS.acceptRequest pending
    case [reqPath pending] `intersect` ["/worker","/browser"] of
      ["/worker"] -> do
        putStrLn "Launching Worker WS"
        hFlush stdout
        myChan <- atomically $ dupTChan jobs
        return ()
      ["/browser"] -> do
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
