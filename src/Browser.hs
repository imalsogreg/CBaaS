module Browser where

import Control.Concurrent.STM
import qualified Network.WebSockets as WS

import Worker

data Browser = Browser
  { bConn       :: WS.Connection
  , bJobResults :: TChan JobResult
  }
