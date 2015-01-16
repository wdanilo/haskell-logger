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
-- WriterLogger
----------------------------------------------------------------------

type Logs m = Seq (Log (LogFormat m))

newtype WriterLogger m a = WriterLogger { fromWriterLogger :: StateT (Logs m) m a } deriving (Monad, MonadIO, Applicative, Functor)

instance MonadTrans WriterLogger where
    lift = WriterLogger . lift

type instance LogFormat (WriterLogger m) = LogFormat m

--runWriterLoggerT :: (Functor m, Monad m) => WriterLogger m b -> m b
runWriterLoggerT = flip runStateT mempty . fromWriterLogger

class MonadWriterLogger m where
    getLogs :: m (Logs m)
    putLogs :: Logs m -> m ()

instance Monad m => MonadWriterLogger (WriterLogger m) where
    getLogs = WriterLogger State.get
    putLogs = WriterLogger . State.put

withLogs f = do
    logs <- getLogs
    putLogs $ f logs

instance (Monad m, Functor m, LogBuilderProto d (WriterLogger m) (LogFormat m), MonadLogger m)
      => MonadRecord d (WriterLogger m)

instance (Functor m, Monad m, MonadLogger m) => MonadLogger (WriterLogger m) where
    appendLog l =  withLogs (|> l)
                *> lift (appendLog l)

instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (WriterLogger m)
