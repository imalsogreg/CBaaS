{-# language OverloadedStrings #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language TemplateHaskell   #-}
{-# language QuasiQuotes       #-}
{-# language TypeFamilies       #-}
{-# language GADTs       #-}
{-# language FlexibleInstances       #-}

module RemoteFunction where

import Data.Aeson
import GHC.Generics
import Data.Text
import Database.Groundhog
import Database.Groundhog.TH
import Model
import Tag

data Function = Function
  { fnType :: Type
  , fnName :: Text
  , fnTags :: [Tag]
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data FunctionTag = FunctionTag
  { ftFunction :: DefaultKey Function
  , ftTag      :: DefaultKey Tag
  }

mkPersist defaultCodegenConfig [groundhog|
 - entity: Function
 - entity: FunctionTag
|]
