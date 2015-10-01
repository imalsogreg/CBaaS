{-# LANGUAGE TypeOperators #-}

module Server.APIServer where

import Servant.API
import Servant.Server
import API

------------------------------------------------------------------------------
-- | Top-level API server implementation
serverAPI :: Server API AppHandler
serverAPI = serverAuth :<|> serverCrud :<|> serverCrud

serverAuth :: Server UserAPI AppHandler
