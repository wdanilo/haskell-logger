{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Drop
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Drop where

import           Data.Monoid
import           Control.Applicative
import           System.Log.Data               (MonadRecord(appendRecord), LogBuilder, LookupDataSet, Msg, Lvl)
import           Control.Lens                  hiding (children)
import           System.Log.Log                (Log, MonadLogger(appendLog), LogFormat, LogFormat)
import           Control.Monad.Trans           (lift)
import           Control.Monad.State           (StateT, runStateT)
import qualified Control.Monad.State           as State
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Text.PrettyPrint.ANSI.Leijen  (Doc, putDoc)
import           System.Log.Logger.Handler     (MonadLoggerHandler, addHandler)
import           System.Log.Logger.Priority    (MonadPriorityLogger, getPriority, setPriority)
import Control.Monad.Trans (MonadTrans)

----------------------------------------------------------------------
-- DropLogger
----------------------------------------------------------------------

newtype DropLoggerT m a = DropLoggerT { runDropLoggerT :: m a } deriving (Monad, MonadIO, Applicative, Functor)

instance MonadTrans DropLoggerT where
    lift = DropLoggerT

type instance LogFormat (DropLoggerT m) = LogFormat m

instance (Monad m, Applicative m) => MonadLogger (DropLoggerT m) where
    appendLog _ = return ()

instance Monad m => MonadRecord d (DropLoggerT m) where
    appendRecord _ = return ()

instance (Monad m, MonadLoggerHandler h m) => MonadLoggerHandler h (DropLoggerT m) where
    addHandler _ = return ()

instance Monad m => MonadPriorityLogger (DropLoggerT m) where
    getPriority   = return undefined
    setPriority _ = return ()
