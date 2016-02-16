{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module ClientGen.JS where

import Data.Proxy
import Data.Text
import Servant.JS

import API

jsCode :: Text
jsCode = jsForAPI (Proxy :: Proxy API1) vanillaJS
