{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

module System.Log.Logger.Thread where

import           System.Log.Data               (MonadRecord, appendRecord, Lvl(Lvl), readData, LevelData(LevelData), LookupDataSet)
import           System.Log.Logger.Handler     (MonadLoggerHandler(addHandler))
import           System.Log.Log                (LogFormat, MonadLogger)
import           System.Log.Logger.Base        (BaseLoggerT, runRawBaseLoggerT)
import           System.Log.Logger.Priority    (MonadPriorityLogger(getPriority,setPriority))
import           Control.Monad.Trans           (MonadTrans, lift)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Applicative
import qualified Control.Monad.Reader          as Reader
import           Control.Monad.Reader          (ReaderT, runReaderT)
import           Control.Concurrent.Chan.Unagi (readChan, writeChan, newChan, InChan, OutChan)
import           Control.Exception             (throwIO, catch, SomeException)
import           Control.Concurrent            (forkIO)
import           Control.Concurrent.MVar       (newEmptyMVar, putMVar, takeMVar)


----------------------------------------------------------------------
-- ThreadedLoggerT
----------------------------------------------------------------------

newtype ThreadedLoggerT' d r m a = ThreadedLoggerT' { fromThreadedLoggerT :: ReaderT (InChan (ChMsg d r)) m a } deriving (Monad, MonadIO, Applicative, Functor, MonadTrans)
type ThreadedLoggerT d m a = ThreadedLoggerT' d a m a
type instance LogFormat (ThreadedLoggerT' d r m) = LogFormat m

data ChMsg m a = ChMsg (m ()) | End a | Exc SomeException

class MonadThreadLogger m n a | m-> n a where
    getLogChan :: m (InChan (ChMsg n a))

-- === Utils ===

runRawThreadedLoggerT :: InChan (ChMsg d r) -> ThreadedLoggerT' d r m a -> m a
runRawThreadedLoggerT ch = flip runReaderT ch . fromThreadedLoggerT

-- cutting out all the logs and sending them over channel, computing result
runRawBaseThreadedLoggerT :: InChan (ChMsg d r) -> ThreadedLoggerT' d r (BaseLoggerT l m) a -> m a
runRawBaseThreadedLoggerT ch = runRawBaseLoggerT . runRawThreadedLoggerT ch

runThreadedLoggerT :: (MonadIO m, Applicative m) => ThreadedLoggerT m (BaseLoggerT l IO) a -> m a
runThreadedLoggerT m = do
    (inChan, outChan) <- liftIO newChan
    liftIO $ forkIO $ do
        out <- (End <$> runRawBaseThreadedLoggerT inChan m) `catch` (return . Exc)
        writeChan inChan out
    loop outChan
    where loop :: (MonadIO m, Applicative m) => OutChan (ChMsg m a) -> m a
          loop ch = do
              l <- liftIO $ readChan ch
              case l of
                  End   a -> return a
                  ChMsg d -> d *> loop ch
                  Exc   e -> liftIO $ throwIO e

liftIOThread :: (MonadIO m, MonadThreadLogger m n a) => (IO () -> IO fa) -> ThreadedLoggerT' n a (BaseLoggerT l IO) b -> m b
liftIOThread f m = do
    inChan <- getLogChan
    ret    <- liftIO $ newEmptyMVar
    liftIO . f $ do
        out <- (End <$> runRawBaseThreadedLoggerT inChan m) `catch` (return . Exc)
        case out of
            End v -> putMVar ret v
            Exc e -> putMVar ret undefined *> writeChan inChan (Exc e)
    liftIO $ takeMVar ret

fork :: (MonadIO m, MonadThreadLogger m n a) => ThreadedLoggerT' n a (BaseLoggerT l IO) b -> m b
fork = liftIOThread forkIO

withTarget :: (MonadThreadLogger m n a, MonadIO m) => n () -> m ()
withTarget f = do
    ch <- getLogChan
    liftIO $ writeChan ch (ChMsg f)

-- === Instances ===

instance Monad m => MonadThreadLogger (ThreadedLoggerT' d r m) d r where
    getLogChan = ThreadedLoggerT' Reader.ask

---

instance (MonadIO m, MonadRecord d n) => MonadRecord d (ThreadedLoggerT' n a m) where
    appendRecord = withTarget . appendRecord

instance (MonadIO m, MonadLoggerHandler h d, LogFormat m ~ LogFormat d) => MonadLoggerHandler h (ThreadedLoggerT' d a m) where
    addHandler = withTarget . addHandler

instance (MonadIO m, MonadPriorityLogger d) => MonadPriorityLogger (ThreadedLoggerT' d a m) where
    setPriority = withTarget . setPriority
    getPriority = error "Cannot get priority from within ThreadLogger!"
