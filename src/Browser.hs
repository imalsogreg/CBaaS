module Browser where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.Map as Map
import Data.UUID
import Data.UUID.V4
import qualified Network.WebSockets as WS

import Worker

data Browser = Browser
  { bID         :: BrowserID
  , bConn       :: WS.Connection
  , bJobResults :: TChan JobResult
  }

newtype BrowserID = BrowserID { unBrowserID :: UUID }
  deriving (Eq, Ord)

newtype BrowserMap = BrowserMap { unBrowserMap :: Map.Map BrowserID Browser }
