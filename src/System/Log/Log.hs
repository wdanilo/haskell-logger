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
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer


----------------------------------------------------------------------
-- Log
----------------------------------------------------------------------

newtype Log a = Log { fromLog :: a } deriving (Show, Functor)

type family LogFormat (m :: * -> *)

type instance LogFormat (ExceptT e m) = LogFormat m
type instance LogFormat (ListT m) = LogFormat m
type instance LogFormat (MaybeT m) = LogFormat m
type instance LogFormat (ReaderT r m) = LogFormat m
type instance LogFormat (RWST r w s m) = LogFormat m
type instance LogFormat (StateT s m) = LogFormat m
type instance LogFormat (WriterT w m) = LogFormat m

----------------------------------------------------------------------
-- MonadLogger
----------------------------------------------------------------------

class (Monad m, Applicative m) => MonadLogger m where
    appendLog :: Log (LogFormat m) -> m ()
