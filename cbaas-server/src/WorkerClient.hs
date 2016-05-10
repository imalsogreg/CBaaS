module WorkerClient where

import           Control.Monad
import qualified Data.Aeson as A
import           Data.ByteString.Lazy.Char8
import           Codec.Picture
import qualified Network.WebSockets as WS
import qualified Model as Model
import           Job
import           Message
-- import           WebSocketServer
import Options.Applicative



--------------------------------------------------------------------
-- Entry point for CBaaS worker nodes
runWorker :: (Model.FromVal a, Model.ToVal b) => (a -> IO b) -> IO ()
runWorker f = WS.runClient "localhost" 9160 "/worker?name=test&function=size" $
  \conn -> forever $ do
    msg <- WS.receive conn
    let t = case msg of
          WS.DataMessage (WS.Text   x) -> Just x
          WS.DataMessage (WS.Binary y) -> Just y
          _                            -> Nothing
    case A.decode =<< t of
        Just (JobRequested (i, j)) -> do
          print "Good decode of job request"
          -- print (Model.fromVal $ _jArg j)
          print "About to send to worker"
          r <- f (Model.fromVal $ _jArg j)
          print "Result: "
          print (Model.toVal r)
          WS.sendTextData conn
            (A.encode $ WorkerFinished (i, (JobResult (Model.toVal r) i)))
        Just x -> print $ "Good decode for unexpected message: " ++ show x
        Nothing -> print $  fmap (("Bad decode of " ++) . unpack ) t
        -- x -> print $ "Got non-text message: " ++ show x
