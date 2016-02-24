{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Server.Application where

------------------------------------------------------------------------------
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Map
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session

import EntityID
import Combo
import Worker
import Browser
import Job
import qualified Model as Model

------------------------------------------------------------------------------
data App = App
    { _heist    :: Snaplet (Heist App)
    , _db       :: Snaplet Postgres
    , _sess     :: Snaplet SessionManager
    , _auth     :: Snaplet (AuthManager App)
    , _workers  :: TVar  (Map (EntityID WorkerProfile) Worker)
    , _browsers :: TVar  BrowserMap
    , _jqueue   :: TChan (EntityID Worker, EntityID Job, Model.Val)
    --, _rqueue   :: TChan (EntityID Worker, EntityID Job, Model.Val)
    , _rqueue   :: TChan (EntityID Job, Maybe (EntityID Browser), JobResult)
    -- , _combo :: Snaplet ComboState -- TODO: having trouble
                                      --       with SnapletInit here
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)


------------------------------------------------------------------------------
type AppHandler = Handler App App


