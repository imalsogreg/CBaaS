{-# LANGUAGE OverloadedStrings #-}

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
import qualified Servant.API as Servant
import Web.HttpApiData

-- import Worker
import Job

data Browser = Browser
  { bID         :: BrowserID
  , bConn       :: WS.Connection
  , bJobResults :: TChan JobResult
  }

newtype BrowserID = BrowserID { unBrowserID :: UUID }
  deriving (Eq, Ord, Show)

newtype BrowserMap = BrowserMap { unBrowserMap :: Map.Map BrowserID Browser }

instance A.ToJSON BrowserID where
  toJSON (BrowserID u) = A.String (toText u)

instance A.FromJSON BrowserID where
  parseJSON (A.String s) = case fromText s of
    Nothing -> mzero
    Just u  -> return $ BrowserID u
  parseJSON _            = mzero

instance FromHttpApiData BrowserID where
  parseUrlPiece s = BrowserID <$> Browser.note "Bad UUID parse" (fromText s)

instance ToHttpApiData BrowserID where
  toUrlPiece (BrowserID u) = toText u

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just a) = Right a


