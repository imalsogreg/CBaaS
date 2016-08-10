{-# language CPP               #-}
{-# language GADTs             #-}
{-# language QuasiQuotes       #-}
{-# language TypeFamilies      #-}
{-# language DeriveGeneric     #-}
{-# language DeriveAnyClass    #-}
{-# language TemplateHaskell   #-}
{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}

module RemoteFunction where

import Data.Aeson
import GHC.Generics
import Data.Text
#ifndef __GHCJS__
import Database.Groundhog
#endif
import Database.Groundhog.Core
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

#ifndef __GHCJS__
mkPersist defaultCodegenConfig [groundhog|
 - entity: Function
 - entity: FunctionTag
|]
#endif
