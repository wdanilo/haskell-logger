-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Level
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Level where

import Prelude hiding (log, lookup)

data Level = Debug     -- ^ Debug Logs
           | Info      -- ^ Information
           | Notice    -- ^ Normal runtime conditions
           | Warning   -- ^ General Warnings
           | Error     -- ^ General Errors
           | Critical  -- ^ Severe situations
           | Alert     -- ^ Take immediate action
           | Panic     -- ^ System is unusable
           deriving (Eq, Ord, Show, Read, Enum)


