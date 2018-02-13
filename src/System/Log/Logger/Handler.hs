{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Logger.Handler
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Logger.Handler where

import           Data.Monoid
import           Control.Applicative
import           System.Log.Data               (MonadRecord(appendRecord), LogBuilder, LookupDataSet, Msg, Lvl)
import           System.Log.Filter             (Filter, runFilter)
import           Control.Lens                  hiding (children)
import           System.Log.Log                (Log, MonadLogger(appendLog), LogFormat, LogFormat)
import           Control.Monad.Trans           (MonadTrans, lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.List
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Control.Monad.State           (StateT, runStateT)
import qualified Control.Monad.State           as State
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           System.Log.Format             (Formatter, runFormatter, defaultFormatter)
import qualified System.IO                     as IO
import           Text.PrettyPrint.ANSI.Leijen  (Doc, putDoc, hPutDoc)

----------------------------------------------------------------------
-- MonadLoggerHandler
----------------------------------------------------------------------

class MonadLoggerHandler n m | m -> n where
    addHandler :: Handler n (LogFormat m) -> m ()

    default addHandler :: (MonadLoggerHandler n m) => Handler n (LogFormat m) -> m ()
    addHandler = addHandler

instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (ExceptT e m)
instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (ListT m)
instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (MaybeT m)
instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (ReaderT r m)
instance (Monad m, Monoid w, MonadLoggerHandler n m) => MonadLoggerHandler n (RWST r w s m)
instance (Monad m, MonadLoggerHandler n m) => MonadLoggerHandler n (StateT s m)
instance (Monad m, Monoid w, MonadLoggerHandler n m) => MonadLoggerHandler n (WriterT w m)

----------------------------------------------------------------------
-- Handler
----------------------------------------------------------------------

-- !!! dorobic formattery i filtracje do handlerow!

data Handler m l = Handler { _name      :: String
                           , _action    :: Doc -> Log l -> m ()
                           , _children  :: [Handler m l]
                           , _formatter :: Maybe (Formatter l)
                           , _filters   :: [Filter l]
                           }
makeLenses ''Handler

type Handler' m = Handler m (LogFormat m)

instance Show (Handler m l) where
    show (Handler n _ _ _ _) = "Handler " <> n

mkHandler :: String -> (Doc -> Log l -> m ()) -> Maybe (Formatter l) -> Handler m l
mkHandler name f fmt = Handler name f [] fmt []
addChildHandler h ph = ph & children %~ (h:)

addFilter :: Filter l -> Handler m l -> Handler m l
addFilter f = filters %~ (f:)

setFormatter :: Formatter l -> Handler m l -> Handler m l
setFormatter f = formatter .~ (Just f)

-- === Handlers ===

topHandler :: Monad m => Formatter l -> Handler m l
topHandler fmt = mkHandler "TopHandler" (\_ _ -> return ()) Nothing
               & formatter .~ (Just fmt)

printHandler :: MonadIO m => Maybe (Formatter l) -> Handler m l
printHandler = mkHandler "PrintHandler" handle where
    handle defDoc l = liftIO $ putDoc defDoc *> putStrLn ""

fileHandler :: MonadIO m => IO.Handle -> Maybe (Formatter l) -> Handler m l
fileHandler h = mkHandler "FileHandler" (handle h) where
    handle h defDoc l = liftIO $ hPutDoc h defDoc *> IO.hPutStrLn h ""

----------------------------------------------------------------------
-- HandlerLoggerT
----------------------------------------------------------------------

newtype HandlerLoggerT m a = HandlerLoggerT { fromHandlerLogger :: StateT (Handler' (HandlerLoggerT m)) m a } deriving (Monad, MonadIO, Applicative, Functor)

type instance LogFormat (HandlerLoggerT m) = LogFormat m

instance MonadTrans HandlerLoggerT where
    lift = HandlerLoggerT . lift

runHandlerLoggerT :: (Functor m, Monad m) => Formatter (LogFormat m) -> HandlerLoggerT m b -> m b
runHandlerLoggerT fmt = fmap fst . flip runStateT (topHandler fmt) . fromHandlerLogger


runHandler :: (Applicative m, Monad m) => Doc -> Log (LogFormat m) -> Handler' m -> m ()
runHandler defDoc l h = act <* mapM (runHandler doc l) (h^.children) where
    flt = runFilters h l
    fmt = h^.formatter
    act = if flt then (h^.action) doc l
                 else return ()
    doc = case fmt of
        Nothing -> defDoc
        Just f  -> runFormatter f l
    runFilters h l = foldr (&&) True $ fmap (\f -> runFilter f l) (h^.filters)


getTopHandler :: Monad m => HandlerLoggerT m (Handler (HandlerLoggerT m) (LogFormat m))
getTopHandler = HandlerLoggerT State.get

putTopHandler :: Monad m => Handler (HandlerLoggerT m) (LogFormat m) -> HandlerLoggerT m ()
putTopHandler = HandlerLoggerT . State.put

-- === Instances ===

instance (MonadLogger m, Functor m, l~LogFormat m, LookupDataSet Msg l, LookupDataSet Lvl l)
      => MonadLogger (HandlerLoggerT m) where
    appendLog l =  (runHandler defDoc l =<< getTopHandler)
                *> lift (appendLog l)
        where defDoc = runFormatter defaultFormatter l

instance (Monad m, Functor m) => MonadLoggerHandler (HandlerLoggerT m) (HandlerLoggerT m) where
    addHandler h = do
        topH <- getTopHandler
        putTopHandler $ addChildHandler h topH

instance (Functor m, MonadLogger m, l~LogFormat m, LogBuilder d (HandlerLoggerT m), LookupDataSet Msg l, LookupDataSet Lvl l)
      => MonadRecord d (HandlerLoggerT m)
