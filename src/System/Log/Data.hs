{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Data
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Data where

import Prelude                hiding (lookup, log)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent     (threadDelay)
import Control.Applicative    hiding (empty)
import System.Log.Log         (MonadLogger, LogFormat, Log(Log), fromLog, appendLog)
import Data.Time.Clock        (getCurrentTime, UTCTime)
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Monoid

----------------------------------------------------------------------
-- Logging utils
----------------------------------------------------------------------


log :: (Show pri, Enum pri, MonadRecord (Data Lvl, (Data Msg, r)) m)
    => RecordBuilder r -> pri -> String -> m ()
log rec pri msg = do
#ifdef NOLOGS
    return ()
#else
    appendRecord $ appData Lvl (mkLevel pri)
                 $ appData Msg msg
                 $ rec
#endif


----------------------------------------------------------------------
-- RecordBuilder
----------------------------------------------------------------------

newtype RecordBuilder a = RecordBuilder { fromRecordBuilder :: a } deriving (Show, Functor)
empty = RecordBuilder ()

class MonadRecord d m where
    appendRecord :: RecordBuilder d -> m ()

    default appendRecord :: (MonadLogger m, LogBuilder d m) => RecordBuilder d -> m ()
    appendRecord d = do
        l <- buildLog d
        appendLog l

instance (Monad m, MonadRecord d m) => MonadRecord d (ExceptT e m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (ListT m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (MaybeT m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (ReaderT s m) where
    appendRecord = lift . appendRecord

instance (Monad m, Monoid w, MonadRecord d m) => MonadRecord d (RWST r w s m) where
    appendRecord = lift . appendRecord

instance (Monad m, MonadRecord d m) => MonadRecord d (StateT s m) where
    appendRecord = lift . appendRecord

instance (Monad m, Monoid w, MonadRecord d m) => MonadRecord d (WriterT w m) where
    appendRecord = lift . appendRecord


appData :: (a~DataOf base) => base -> a -> RecordBuilder as -> RecordBuilder (Data base, as)
appData base a = fmap (Data base a,)


----------------------------------------------------------------------
-- Basic data wrappers
----------------------------------------------------------------------

data Data base = Data { recBase :: base
                      , recData :: DataOf base
                      }
deriving instance (Show (DataOf base), Show base) => Show (Data base)

class DataGetter base m where
    getData :: m (Data base)

type family DataOf a :: *


----------------------------------------------------------------------
-- LogBuilder
----------------------------------------------------------------------

class LogBuilderProto a m b where
    buildLogProto :: RecordBuilder a -> m (Log b)

type LogBuilder a m = LogBuilderProto a m (LogFormat m)

buildLog :: (Monad m, Applicative m, LogBuilder a m) => RecordBuilder a -> m (Log (LogFormat m))
buildLog = buildLogProto

-- === Instances ===

instance (LogBuilderProto xs m ys, Functor m) => LogBuilderProto (Data x,xs) m (Data x,ys) where
    buildLogProto b = (fmap.fmap) (x,) $ buildLogProto $ RecordBuilder xs where
        (x,xs) = fromRecordBuilder b

instance (LogBuilderProto (Data x,xs) m ys, LogBuilderProto xs m (Data y,()), Monad m) => LogBuilderProto (Data x,xs) m (Data y,ys) where
    buildLogProto b = do
        let (x,xs) = fromRecordBuilder b
        Log ys     <- buildLogProto b
        Log (y,()) <- buildLogProto $ RecordBuilder xs
        return $ Log (y, ys)

instance Monad m => LogBuilderProto a m () where
    buildLogProto _ = return $ Log ()

instance (Functor m, Applicative m, DataGetter y m, LogBuilderProto () m ys) => LogBuilderProto () m (Data y,ys) where
    buildLogProto b = fmap Log $ (,) <$> getData <*> (fromLog <$> buildLogProto b)


----------------------------------------------------------------------
-- Data reading
----------------------------------------------------------------------

class Lookup base s where
    lookup :: base -> s -> Data base

readData :: Lookup a l => a -> l -> DataOf a
readData a = recData . lookup a

-- === Instances ===

instance LookupDataSet base l => Lookup base (Log l) where
    lookup b (fromLog -> s) = lookupDataSet b s

instance LookupDataSet base r => Lookup base (RecordBuilder r) where
    lookup b (fromRecordBuilder -> r) = lookupDataSet b r

---

class LookupDataSet base s where
    lookupDataSet :: base -> s -> Data base

instance LookupDataSet base (Data base,as) where
    lookupDataSet _ (a,_) = a

instance LookupDataSet base as => LookupDataSet base (Data b,as) where
    lookupDataSet b (_, as) = lookupDataSet b as


----------------------------------------------------------------------
-- Simple data providers
----------------------------------------------------------------------

-- Time --

data Time = Time deriving (Show)
type instance DataOf Time = UTCTime

instance MonadIO m => DataGetter Time m where
    getData = do liftIO $ Data Time <$> getCurrentTime

-- Msg --

data Msg = Msg deriving (Show)
type instance DataOf Msg = String


-- Lvl --

data Lvl = Lvl deriving (Show)
type instance DataOf Lvl = LevelData
data LevelData = LevelData Int String deriving (Show, Ord, Eq)
mkLevel a = LevelData (fromEnum a) (show a)

-- Loc --

type Pos = (Int, Int)

data LocData = LocData { _filename :: String
                       , _package  :: String
                       , _module   :: String
                       , _start    :: Pos
                       , _end      :: Pos
                       } deriving Show

mkLoc (f,p,m,s,e) = LocData f p m s e

data Loc = Loc deriving (Show)
type instance DataOf Loc = LocData

makeLenses ''Loc
