{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Simple
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Simple (
	module System.Log.Simple,
	module X
) where

import System.Log.Level           as X
import System.Log.Log             as X
import System.Log.Data            as X
import System.Log.Format          as X
import System.Log.Logger.Base     as X
import System.Log.Logger.Handler  as X
import System.Log.Logger.Priority as X
import System.Log.Logger.Thread   as X
import System.Log.Logger.Drop     as X
import System.Log.Filter          as X
import System.Log.Logger.Writer   as X

import Prelude          hiding (log)
import System.Log.Level
import System.Log.Data  (log, empty)

simpleLog = log empty

debug     = simpleLog Debug
info      = simpleLog Info
notice    = simpleLog Notice
warning   = simpleLog Warning
error     = simpleLog Error
critical  = simpleLog Critical
alert     = simpleLog Alert
panic     = simpleLog Panic