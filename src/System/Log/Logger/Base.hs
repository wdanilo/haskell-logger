{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Base
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Base where

import Control.Monad.Trans
import System.Log.Data (Data, MonadRecord(appendRecord))
import Control.Applicative
import System.Log.Tuples
import System.Log.Log (LogFormat, MonadLogger, appendLog)
import Control.Monad.Identity (runIdentity)

----------------------------------------------------------------------
-- BaseLoggerT
----------------------------------------------------------------------

newtype BaseLoggerT l m a = BaseLoggerT { runRawBaseLoggerT :: m a } deriving (Monad, MonadIO, Applicative, Functor) 

runBaseLoggerT :: (Functor m, Monad m) => l -> BaseLoggerT (MapRTuple Data (Tuple2RTuple l)) m a -> m a
runBaseLoggerT _ = runRawBaseLoggerT

runBaseLogger d = runIdentity . runBaseLoggerT d

-- === instances ===

type instance LogFormat (BaseLoggerT l m) = l


instance (Applicative m, Monad m) => MonadLogger (BaseLoggerT l m) where
	appendLog _ = return ()

instance MonadTrans (BaseLoggerT l) where
    lift = BaseLoggerT

instance Monad m => MonadRecord d (BaseLoggerT l m) where
    appendRecord _ = return ()


