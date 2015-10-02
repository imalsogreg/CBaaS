module APIDocs where

import Servant
import qualified Servant.Docs as Docs

import API
import Types

apiDocs :: Docs.API
apiDocs = Docs.docs (Proxy :: Proxy API1)
