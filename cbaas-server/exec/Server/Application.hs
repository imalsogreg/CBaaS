{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Server.Application where

------------------------------------------------------------------------------
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Map
import Data.Pool
import qualified Database.Groundhog as G
import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Postgresql as G
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
-- import Snap.Snaplet.PostgresqlSimple
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
--    , _db       :: Snaplet Postgres
    , _db       :: Pool G.Postgresql
    , _sess     :: Snaplet SessionManager
    , _auth     :: Snaplet (AuthManager App)
    , _workers  :: TVar  (Map (EntityID WorkerProfile) Worker)
    , _browsers :: TVar  (Map BrowserProfileId Browser)
    , _jqueue   :: TChan (EntityID Worker, EntityID Job, Model.Val)
    --, _rqueue   :: TChan (EntityID Worker, EntityID Job, Model.Val)
    , _rqueue   :: TChan (EntityID Job, Maybe (EntityID BrowserProfile), JobResult)
    -- , _combo :: Snaplet ComboState -- TODO: having trouble
                                      --       with SnapletInit here
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

-- instance HasPostgres (Handler b App) where
--   getPostgresState = with db get
--   setLocalPostgresState s = local (set (db . snapletValue) s)

-- instance G.ConnectionManager App (Pool G.Postgresql) where
--   withConn f pconn = withResource pconn $ G.withConn f . G.Postgresql
--   withConnNoTransaction f pconn =
--      withResource pconn $ G.withConnNoTransaction f . G.Postgresql

instance G.ConnectionManager App G.Postgresql where
  withConn f app              = G.withConn f (_db app)
  withConnNoTransaction f app = G.withConnNoTransaction
                                 f (_db app)

runGH :: G.ConnectionManager b conn
      => G.DbPersist conn (NoLoggingT IO) a
      -> Handler b v a
runGH f = withTop' id $ do
  cm <- ask
  liftIO $ runNoLoggingT (G.withConn (G.runDbPersist f) cm)



------------------------------------------------------------------------------
type AppHandler = Handler App App


