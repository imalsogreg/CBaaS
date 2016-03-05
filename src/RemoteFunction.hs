{-# language OverloadedStrings #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language TemplateHaskell   #-}
{-# language QuasiQuotes       #-}

module RemoteFunction where

import Data.Aeson
import GHC.Generics
import Data.Text
import Database.Groundhog.TH
import Model
import Tag

data Function = Function
  { _fnType :: Type
  , _fnName :: Text
  , _fnTags :: [Tag]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


-- mkPersist defaultCodegenConfig [groundhog|
--  - entity: Function
-- |]
