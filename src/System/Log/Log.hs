{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Log
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Log where

import Control.Applicative


----------------------------------------------------------------------
-- Log
----------------------------------------------------------------------

newtype Log a = Log { fromLog :: a } deriving (Show, Functor)

type family LogFormat (m :: * -> *)

----------------------------------------------------------------------
-- MonadLogger
----------------------------------------------------------------------

class (Monad m, Applicative m) => MonadLogger m where
    appendLog :: Log (LogFormat m) -> m ()








