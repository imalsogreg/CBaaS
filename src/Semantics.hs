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

import Job
import Worker
import EntityID
import Browser

data CBaaSF a
  = BrowserJoin (Browser -> a)
  | BrowserLeave a
  | WorkerJoin WorkerProfile (Worker -> a)
  | RequestWork Browser Worker Job a
  | ReportWorkFinished Worker Job (JobResult -> a)
  deriving (Functor)

type CBaaS = Free CBaaSF

-- makeFree ''CBaaSF
-- Lots of errors here


simulateCBaaSIO :: CBaaS a -> IO a
simulateCBaaSIO = do
  undefined
