{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Server.Application where

------------------------------------------------------------------------------
import Control.Concurrent.STM
import Control.Exception (onException)

import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Bool (bool)
import Data.Functor.Contravariant
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Pool
import qualified Database.Groundhog as G
import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Postgresql as GP
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session

import EntityID
import Worker
import WorkerProfile
import BrowserProfile
import Browser
import Job
import qualified Model as Model

------------------------------------------------------------------------------
data App = App
    { _heist    :: Snaplet (Heist App)
    , _db       :: Pool GP.Postgresql -- Snaplet (Pool GP.Postgresql)
    , _sess     :: Snaplet SessionManager
    , _auth     :: Snaplet (AuthManager App)
    , _workers  :: TVar  (Map (EntityID WorkerProfile) Worker)
    , _browsers :: TVar  (Map BrowserProfileId Browser)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

-- instance G.ConnectionManager App where
--   withConn f app = do
--     let g (pg :: GP.Postgresql) = undefined -- \f -> f (app { _db = pg})
--     let g' = _
--     myPg@(GP.Postgresql c) :: GP.Postgresql <- G.extractConn return (_db app)
--     G.withConn g' myPg
--     -- conn@(GP.Postgresql c) <- G.extractConn return (_db app)
--     -- liftIO $ PG.begin c
--     -- x <- onException (f app) (liftIO $ PG.rollback c)
--     -- liftIO $ PG.commit c
--     -- return x

-- instance G.ConnectionManager App where
--   withConn :: forall m a. (MonadBaseControl IO m, MonadIO m) => (App -> m a) -> App -> m a
--   withConn (f :: App -> m a)  (app@(App _ mydb _ _ _ _) :: App) = do
--     let g :: GP.Postgresql -> m a = _ f
--     a <- f app
--     return a

runGH :: G.DbPersist GP.Postgresql (NoLoggingT IO) a
      -> Handler App App a
runGH f =  do
  cm <- reader _db
  pgConn <- G.extractConn return cm
  liftIO $ runNoLoggingT (G.withConn (G.runDbPersist f) pgConn)



------------------------------------------------------------------------------
type AppHandler = Handler App App
