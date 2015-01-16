{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Handler
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Priority where

import System.Log.Data           (MonadRecord, appendRecord, Lvl(Lvl), readData, LevelData(LevelData), LookupDataSet)
import System.Log.Logger.Handler (MonadLoggerHandler(addHandler))
import System.Log.Log            (MonadLogger, LogFormat)
import Control.Monad.State       (StateT, runStateT)
import Control.Monad.Trans       (MonadTrans, lift)
import Control.Monad.IO.Class    (MonadIO)
import Control.Applicative
import qualified Control.Monad.State as State

----------------------------------------------------------------------
-- MonadPriorityLogger
----------------------------------------------------------------------

class MonadPriorityLogger m where
    getPriority :: m Int
    setPriority :: Enum a => a -> m ()

----------------------------------------------------------------------
-- PriorityLoggerT
----------------------------------------------------------------------

newtype PriorityLoggerT m a = PriorityLoggerT { fromPriorityLoggerT :: StateT Int m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)
type instance LogFormat (PriorityLoggerT m) = LogFormat m

runPriorityLoggerT pri = fmap fst . flip runStateT (fromEnum pri) . fromPriorityLoggerT


-- === Instances ===

instance Monad m => MonadPriorityLogger (PriorityLoggerT m) where
    getPriority   = PriorityLoggerT State.get
    setPriority a = PriorityLoggerT . State.put $ fromEnum a

instance (MonadLogger m, MonadRecord d m, LookupDataSet Lvl d) => MonadRecord d (PriorityLoggerT m) where
    appendRecord d = do
        priLimit <- getPriority
        let LevelData pri _ = readData Lvl d
        if priLimit <= pri then lift $ appendRecord d
                           else return ()

instance (Monad m, MonadLoggerHandler h m) => MonadLoggerHandler h (PriorityLoggerT m)