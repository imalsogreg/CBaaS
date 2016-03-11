{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell #-}

module Combo where

import           Control.Applicative          hiding (empty)
import           Control.Monad.Base
-- import           Control.Monad.Logger
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Char8         as B
import qualified Data.Configurator             as C
import qualified Data.Configurator.Types       as C
import           Control.Lens
import           Data.Map                      (Map, empty)
import           Data.Monoid
import           Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import           Snap.Snaplet.PostgresqlSimple (Postgres(..),
                                                getConnectionString)

import           Worker

-- newtype Combo m a = Combo { unCombo :: ReaderT ComboState m a }
--   deriving (MonadReader ComboState, Monad, MonadIO,
--            MonadTrans, Functor, Applicative)


-- data ComboState = ComboState
--   { _csPG      :: Pool PG.Connection
--   , _csWorkers :: TVar (Map WorkerID Worker)
--   }

-- makeLenses ''ComboState

-- mkComboStateEnv :: String -> IO ComboState
-- mkComboStateEnv env = do
--   conf <- C.load [C.Required (env <> ".cfg")]
--   mkComboState conf

-- mkComboState :: C.Config -> IO ComboState
-- mkComboState conf = do
--   let cfg = C.subconfig "postgresql-simple" conf
--   connstr   <- liftIO $ getConnectionString cfg
--   stripes   <- C.lookupDefault 1 cfg "numStripes"
--   idle      <- C.lookupDefault 5 cfg "idleTime"
--   resources <- C.lookupDefault 20 cfg "maxResourcesPerStripe"
--   pool      <- createPool (PG.connectPostgreSQL connstr) PG.close stripes
--                (realToFrac (idle :: Double)) resources

--   workerMap <- newTVarIO empty

--   return $ ComboState pool workerMap
