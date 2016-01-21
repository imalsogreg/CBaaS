{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module ClientGen.Matlab where

import Data.Proxy
import Servant
import Servant.Matlab
import Servant.Matlab.Functions

import API

matlabCode :: [(String,String)]
matlabCode = matlabForAPI (Proxy :: Proxy API1) matlabFunctions
