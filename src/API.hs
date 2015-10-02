{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API where

------------------------------------------------------------------------------
import           Control.Monad (mzero)
import qualified Data.Aeson    as A
import           Data.Aeson    ((.:), (.=), ToJSON(..), FromJSON(..))
import           Data.Text     (Text, pack, unpack)
import qualified Data.UUID     as UUID
import           Data.UUID     (UUID, fromText, toText)
import           Servant.API   ((:>), (:<|>), Get, Post, Put, Delete, JSON
                               ,Capture, ReqBody, Raw, FormUrlEncoded)
import           Types


------------------------------------------------------------------------------
-- | This is the API definition of CBaaS.
--   We use it to specify the activity of the server, and also to
--   generate documentation and clients in a number of languages
--   For more information about API specifications, see the Servant
--   <http://haskell-servant.github.io documentation>
type API1 =             
                    "stimulus"      :> CrudAPI EntityID StimulusResource
                    :<|> "feature"  :> CrudAPI EntityID Features
                    :<|> "user"     :> UserAPI


------------------------------------------------------------------------------
-- | User session sub-api
--   Clients and this sites pages use this API for user and session management
type UserAPI =
            "login" :> ReqBody '[FormUrlEncoded, JSON] LoginInfo :> Post '[JSON] ()

            :<|> "register" :> ReqBody '[FormUrlEncoded, JSON] RegisterInfo :> Post '[JSON] ()

            :<|> "user" :> Get '[JSON] User

            :<|> "logout" :> Post '[JSON] ()


------------------------------------------------------------------------------
-- | A generic API for Creating, Reading, Updating, and Deleting values
--   of type `v` indexed by values of type `i`. We can reuse this API
--   for any new types we come up with
type CrudAPI i v =
  Capture "id"    i :> Get     '[JSON] v
  :<|> ReqBody '[JSON] v :> Post    '[JSON] i
  :<|> Capture "id"    i :> ReqBody '[JSON] v :> Put '[JSON] i
  :<|> Capture "id"    i :> Delete  '[JSON] i
