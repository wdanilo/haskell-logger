{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Filter
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Filter where

import System.Log.Log (Log)
import System.Log.Data  (Lvl(Lvl), Msg(Msg), LevelData(LevelData), readData, DataOf, Lookup, LookupDataSet)

----------------------------------------------------------------------
-- Filter
----------------------------------------------------------------------

newtype Filter a = Filter { runFilter :: Log a -> Bool }

lvlFilter' :: (LookupDataSet Lvl l, Enum a) => a -> Log l -> Bool
lvlFilter' lvl l = (i >= fromEnum lvl) where
    LevelData i _ = readData Lvl l

lvlFilter lvl = Filter (lvlFilter' lvl)
