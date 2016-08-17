{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Server.Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad (join)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Data.ByteString (ByteString)
import qualified Data.Configurator as C
import           Data.Map (empty)
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Database.Groundhog as G
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Postgresql
import           Servant.API hiding (GET, POST, PUT, DELETE)
import           Servant.Server
import           Servant.Server.Internal.SnapShims (applicationToSnap)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           API
import           Job
import           EntityID
import           RemoteFunction
import           WorkerProfile
import           Server.Application
import           Server.APIServer
import           Server.GroundhogAuth
import           Server.WebSocketServer

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login"   , with auth handleLoginSubmit)
         , ("test"    , writeBS "Test handler")
         , ("logout"  , with auth handleLogout)
         , ("new_user", with auth handleNewUser)
         , ("api1"    , applicationToSnap (serve (Proxy :: Proxy API1) serverAPI))
         , (""        , serveDirectory "static" )
         , (""        , render "function") -- TODO
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    cfg <- getSnapletUserConfig
    cfg <- C.subconfig "postgresql" <$> getSnapletUserConfig

    connstr <- liftIO $ decodeUtf8 <$> getConnectionString cfg
    p   <- liftIO $ withPostgresqlPool (T.unpack connstr) 3 return
    c   <- liftIO $ extractConn (return) p
    liftIO $ print connstr
    liftIO $ runNoLoggingT (withConn (runDbPersist migrateDB) c)

    h   <- nestSnaplet "" heist $ heistInit "templates"
    s   <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    a   <- nestSnaplet "auth" auth $
             initGroundhogAuth sess p
    w   <- liftIO $ newTVarIO Data.Map.empty
    b   <- liftIO $ newTVarIO Data.Map.empty
    r   <- liftIO newTChanIO

    addRoutes routes
    return $ App h p s a w b


migrateDB :: (MonadIO m, PersistBackend m) => m ()
migrateDB = runMigration $ do
      G.migrate (undefined :: Function)
      G.migrate (undefined :: WorkerProfile)
      G.migrate (undefined :: JobResult)
