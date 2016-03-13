{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------------------------------
-- |
-- Module       : Semantics
-- Copyright    : (c) Greg Hale 2016
-- License      : BSD3 (see the file LICENSE)
--
-- Maintainer   : Greg Hale <imalsogreg@gmail.com>
-- Stability    : experimental
-- Portability  : template haskell
--
-- The semantics of the CBaaS network (data proxy servers, browsers, 
-- and workers) expressed as a Free Monad. The goal is to make the
-- operation of the network clear. Eventually it would be nice to write
-- the server itself as an interpreter for this module. Other uses for this
-- may be a testing setup, or an demo animation.
--
-- Also this is my time to try to learn about free monad use :)
------------------------------------------------------------------------------

module Semantics where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.IORef
import Data.UUID.V4
import Data.Map (Map)
import qualified Data.Map as Map

import BrowserProfile
import EntityID
import Job
import WorkerProfile

data CBaaSF a
  = BrowserJoin (BrowserProfile -> a)
  | BrowserLeave a
  | WorkerJoin WorkerProfile (WorkerProfile -> a)
  | RequestWork BrowserProfile WorkerProfile Job a
  | ReportWorkFinished WorkerProfile Job (JobResult -> a)
  deriving (Functor)

type CBaaS = Free CBaaSF



simulateCBaaSIO :: CBaaS a -> IO a
simulateCBaaSIO = do
  undefined
