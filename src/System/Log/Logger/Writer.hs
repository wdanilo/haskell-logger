{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Writer
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Writer where

import           Data.Monoid
import           Data.Sequence          (Seq, (|>))
import           System.Log.Log         (Log, MonadLogger, LogFormat, appendLog)
import           System.Log.Logger.Handler (MonadLoggerHandler)
import qualified Control.Monad.State    as State
import           Control.Monad.State    (StateT, runStateT)
import           Control.Applicative
import           Control.Monad.IO.Class (MonadIO)
import           System.Log.Data        (MonadRecord, LogBuilderProto)
import           Control.Monad.Trans    (MonadTrans, lift)

----------------------------------------------------------------------
-- WriterLoggerT
----------------------------------------------------------------------

type Logs m = Seq (Log (LogFormat m))

newtype WriterLoggerT m a = WriterLoggerT { fromWriterLoggerT :: StateT (Logs m) m a } deriving (Monad, MonadIO, Applicative, Functor)

instance MonadTrans WriterLoggerT where
    lift = WriterLoggerT . lift

type instance LogFormat (WriterLoggerT m) = LogFormat m

--runWriterLoggerT :: (Functor m, Monad m) => WriterLoggerT m b -> m b
runWriterLoggerT = flip runStateT mempty . fromWriterLoggerT

class MonadWriterLogger m where
    getLogs :: m (Logs m)
    putLogs :: Logs m -> m ()

instance Monad m => MonadWriterLogger (WriterLoggerT m) where
    getLogs = WriterLoggerT State.get
    putLogs = WriterLoggerT . State.put

withLogs f = do
    logs <- getLogs
    putLogs $ f logs

instance (Monad m, Functor m, LogBuilderProto d (WriterLoggerT m) (LogFormat m), MonadLogger m)
      => MonadRecord d (WriterLoggerT m)

instance (Functor m, Monad m, MonadLogger m) => MonadLogger (WriterLoggerT m) where
    appendLog l =  withLogs (|> l)
                *> lift (appendLog l)

instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (WriterLoggerT m)
