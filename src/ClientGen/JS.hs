{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module ClientGen.JS where

import Data.Proxy
import Servant
import Servant.JS

import API

jsCode :: String
jsCode = jsForAPI (Proxy :: Proxy API1) vanillaJS
