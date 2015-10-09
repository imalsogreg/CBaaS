{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module ClientGen.Matlab where

import Data.Proxy
import Servant
import Servant.Matlab

import API

matlabCode :: String
matlabCode = matlabForAPI (Proxy :: Proxy API1) matlabFunctions
