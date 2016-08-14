{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}

module ClientGen.Matlab where

import Data.Proxy
import Data.Text
import Servant.API
-- import Servant.Matlab
-- import Servant.Matlab.Functions

import API

matlabCode :: [(Text,Text)]
matlabCode = [] -- matlabForAPI (Proxy :: Proxy API1) matlabFunctions
