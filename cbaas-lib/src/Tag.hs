{-# language CPP       #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language TemplateHaskell   #-}
{-# language QuasiQuotes       #-}
{-# language TypeFamilies       #-}
{-# language GADTs       #-}
{-# language FlexibleInstances       #-}

module Tag where

import Data.Aeson
import Data.Text
#ifndef __GHCJS__
import Database.Groundhog
import Database.Groundhog.Generic
#endif
import Database.Groundhog.TH
import GHC.Generics

data Tag = Tag { tagName :: Text }
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

#ifndef __GHCJS__
mkPersist defaultCodegenConfig [groundhog|
 - entity: Tag
|]
#endif
