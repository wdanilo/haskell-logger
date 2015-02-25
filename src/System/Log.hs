-----------------------------------------------------------------------------
{- |
   Module      :  System.Log
   Copyright   :  (C) 2015 Flowbox
   License     :  Apache-2.0
   Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
   Stability   :  stable
   Portability :  portable

Written by Wojciech Daniło @ Flowbox.io

= Overview

Logging is a fast and extensible Haskell logging framework. 

Logging allows you to log any kind of messages in both IO as well as pure code, depending on the informations you want to log.

The framework bases on the idea of logger transformer stack defining the way it works. You can build your own stack to highly tailor the behaviour to your needs, starting with such simple things, like logging messages to a list, ending on logging compile-time, priority-filtered messages from different threads and gathering them in other logger thread.

= Documentation

The following documentation describes how to use the framework, how it works under the hood and how can you extend it.

== Basics

This chapter covers all the basic information about logger transformers shipped with the framework.

=== BaseLogger

Let's start with a very simple example:

@
import System.Log.Simple

test = do
    debug "a debug"
    warning "a warning"
    return "Done"

main = print $ runBaseLogger (Lvl, Msg) test
-- output: "Done"
@

There are few things to note here:

    * We are importing the ''System.Log.Simple'' interface. It provides all necessary functions to start with the library. There is other interface, ''System.Log.TH'', which provides simmilar functionality, but allows additionally logging such informations like file or module name and log location inside the file.
    * We are running the logger using 'runBaseLogger' function providing the description what type of information we want to gather with each call to 'debug', 'warning', etc. This is very important, because we can choose only the needed information, like messages and levels and run the logger as a pure code. If you try to run the example with other description, like @(Lvl, Msg, Time)@, it will fail complaining that it needs the 'IO' monad for that.
    * The 'BaseLogger' is the most base logger transformer and it should be run as a base for every logger transformer stack. It do not log any messages under the hood, in fact you cannot do anything sensible with it.

As every logger transformer, 'BaseLogger' has an appriopriate transformer type called 'BaseLoggerT'. You can use it just as every monad transformer, to pipe computations to an underlying monad. Using the transformer we can ask our logger to log also such information as the time:

@
main = print =<< runBaseLogger (Lvl, Msg, Time) test
@

There is one very important design decision. All the logger transformers, appart from the base one, pass the newly registered log to underlying transformers. This way we can create a transformer that writes messages to disk and combine it with the one, that registers the logs in a list. There are some examples showing this behavior later in this document.

=== WriterLogger

'WriterLogger' is just like 'Writer' monad - it gathers all the logs into a list and returns it:
@
main = print $ (runBaseLogger (Lvl, Msg) . runWriterLoggerT) test
@
As a result we get tuple, which first element is the functions return value, while the second is list of all Log messages. For now the log message is not very friendly nested-tuple structure, but it will change in the next versions of the library. To be clear, the single log looks like this at the moment:
@
Log {fromLog = (Data {recBase = Lvl, recData = LevelData 0 "Debug"},(Data {recBase = Msg, recData = "a debug"},()))}
@

WiterLogger should work as fast as just 'WriterT' monad transformer with 'Dlist' used for logs gathering, because there should be no overhead introduced by the library.

=== HandlerLogger

'HandlerLogger' allows you to handle messages using handlers and log formatters. At last we will see something usefull as a logging library! To start, let's look at a simple example:

@
import System.Log.Simple

test = do
    addHandler $ printHandler Nothing
    debug "a debug"
    warning "a warning"

main = print =<< (runBaseLoggerT (Lvl, Msg) . runHandlerLoggerT defaultFormatter) test
@

As a result, we get a colored output (on all platforms, including Windows):

@
[Debug] a debug
[Warning] a warning
"Done"
@

Ok, so what's happening here? The function 'addHandler' registers new log handler in current logger monad. The @Nothing@ just indicates, that this handler does not need any special formatter and can use the default one, provided when executing the monad - in this case, the 'defaultFormatter'. We can of course define our custom message formatters.

For no only the 'printHandler' is provided, but it is straightforward to define custom handlers. Other will be added in the next versions of the library.

==== Formatters

It is possible to define a custom message formatter. To do it, import the module ''System.Log.Format'' and use so called formatter builder. Let's see how the 'defaultFormatter' is defined:

@
defaultFormatter = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Msg
@

You might ask now, what are 'Lvl' or 'Msg'. They are "data pointers". You will learn about them later, for now just remember, you can use them while running loggers as well as defining formatters. There is one very important thing to note here - you cannot use any data provider in your logger, that was not declared to be gathered when the logger is run! In later chapters you will also learn how to create custom data providers.

So what if we would like to output not only the message and it's priority level, but also the module name and location of the message in the source file? Such logger is also defined and it's called 'defaultFormatterTH'. You cannot use it using the 'Simple' interface, so lets see for now how it is defined:

@
defaultFormatterTH = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Loc <:> ": " <:> Msg
@

It's output is simmilar to:

@
[Debug] Main.hs:4: a debug
[Warning] Main.hs:5: a warning
@

=== PriorityLogger

The 'PriorityLogger' is used to filter the messages by priority levels. It is important to note here, that 'PriorityLogger' is able to filter them at compile time, so if we need some IO actions to construct a log, like reading a time or process id, they will not be executed when the priority of such log is too low. Let's see how we can use it:

@
test = do
    addHandler $ printHandler Nothing
    debug "a debug"
    setPriority Debug
    debug "another debug"
    warning "a warning"
    
print =<< ( runBaseLoggerT (Lvl, Msg) 
          . runHandlerLoggerT defaultFormatter 
          . runPriorityLoggerT Warning 
          ) test
@

As the output we get:

@
[Debug] another debug
[Warning] a warning
@

=== ThreadedLogger

The 'ThreadedLogger' is a very fancy one. It allows separate the actual logging from program. Program is being run on a separate thread, while logs are being gathered by the main thread. You can fork the program as many times you want and all the logs will be send to the log-gather routine. This allows to get nicely not-broken output in terminal or in files from different threads. The program stops after all the logs have been processed. Lets look at the example:

@
import           System.Log.Simple
import qualified System.Log.Logger.Thread as Thread
import           Control.Monad.IO.Class (liftIO)

test = do
    addHandler $ printHandler Nothing
    debug "a debug"
    setPriority Debug
    debug "another debug"
    warning "a warning"
    Thread.fork $ do
        liftIO $ print "Threaded print"
        debug "debug in fork"
    liftIO $ print "End of the test!"

print =<< ( runBaseLoggerT (Lvl, Msg)
          . runHandlerLoggerT defaultFormatter
          . runPriorityLoggerT Warning
          . runThreadedLoggerT
          ) test
@

As the output we get:

@
"Threaded print"
"End of the test!"
[Debug] another debug
[Warning] a warning
[Debug] debug in fork
@

The output may of course vary, based on the way threads will be sheduled, because we use 'print' functions here. Anyway you can notice, that the prints were executed at the same time as all the logging.
It is important to use @Thread.fork@, which is just a simple wrapper around 'forkIO'.

==== Exception handling

All the loggers behave in a proper way, when an exception is rised. The exception will be evaluated after all necessary logging has been done:

@
test = do
    addHandler $ printHandler Nothing
    debug "debug"
    Thread.fork $ do
        fail "oh no"
        debug "debug in fork"
    warning "a warning"

print =<< ( runBaseLoggerT (Lvl, Msg)
          . runHandlerLoggerT defaultFormatter
          . runThreadedLoggerT
          ) test
@

Results in:

@
[Debug] debug
Main.hs: user error (oh no)
@

=== DropLogger

The 'DropLogger' allows you to simply drop all logs from the function. It could be used if you want to execute a subroutine but just discard all logging there. The log messages would be completely discarded - they will not even be created.

== TemplateHaskell interface

You can use more advanced interface to be able to log more information, like module name or file number. To use it, import @System.Log.TH@ instead of @System.Log.Simple@ and use TemplateHaskell syntax to report logs:

@
import System.Log.TH

test = do
    addHandler $ printHandler Nothing
    $(debug "a debug")
    setPriority Debug
    $(debug "another debug")
    $(warning "a warning")

print =<< ( runBaseLoggerT (Lvl, Msg, Loc)
          . runHandlerLoggerT defaultFormatterTH
          . runPriorityLoggerT Warning
          . runThreadedLoggerT
          ) test
@

Which results in the following output:

@
[Debug] Main:7: another debug
[Warning] Main:8: a warning
@

== Filtering messages

The framework allows you to filter messages after they have been created. It is slower than using 'PriorityLogger' because the messages are created even if they are not needed. It could be used for example in a situation, where you've got many handlers and you want to output only important logs to the screen and all the logs into files. Here's a small example showing how it works.

@
test = do
    addHandler $ addFilter (lvlFilter Warning) $ printHandler Nothing
    $(debug "a debug")
    $(warning "a warning")

print =<< ( runBaseLoggerT (Lvl, Msg, Loc) 
          . runHandlerLoggerT defaultFormatterTH
          ) test
@

Which results in:

@
[Warning] Main:5: a warning
@

== Extending the logger

It is possible to extend the logging framework in any way you want. All the functionality you have seen above are just simple logger transformers and you can modify them in a ton of ways or create custom ones.

=== Custom prioritiy levels

Defining a custom priority levels is as easy as creating a new datatype that derives the 'Enum' and start using it. The default prorities are defined as:

@
data Level = Debug     -- ^ Debug Logs
           | Info      -- ^ Information
           | Notice    -- ^ Normal runtime conditions
           | Warning   -- ^ General Warnings
           | Error     -- ^ General Errors
           | Critical  -- ^ Severe situations
           | Alert     -- ^ Take immediate action
           | Panic     -- ^ System is unusable
           deriving (Eq, Ord, Show, Read, Enum)
@

=== Custom data providers

It is possible to define custom data providers. Let's look how the 'Msg' data provided is defined in the library:

@
data Msg = Msg deriving (Show)
type instance DataOf Msg = String
@

That's it. There is no more code for it. After creating such new datatype you can create a pretty printing instance for it and use it just like all other data even in the formatter builder!
But how the data is being registered? Let's look how the 'debug' function is defined in the 'Simple' library:

@
debug = log empty Debug
@

The 'log' function is a very generic one and allows creating almost any logging functionality. If for example we would love to add a new data provider 'Foo' registering an 'Int', we can do this simply by:

@
data Foo = Foo deriving (Show)
type instance DataOf Foo = Int

debugFoo i = log (appData Foo i empty) Debug

instance PPrint Foo where
    pprint = text . show

fooFormatter = defaultFormatter <:> " (" <:> Foo <:> ")"

test = do
    addHandler $ printHandler Nothing
    debugFoo 7 "my custom debug"

print =<< ( runBaseLoggerT (Lvl, Msg, Foo) 
          . runHandlerLoggerT defaultFormatter
          ) test
@

Which results in:

@
[Debug] my custom debug (7)
@

==== monad data providers

What happens when such data is not provided when constructing the message? Like 'Time' data? If data is not available at construction time, the logger looks for its 'DataGetter' instance. A simple 'Time' data provider could be defined as:

@
import Data.Time.Clock  (getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data Time = Time deriving (Show)
type instance DataOf Time = UTCTime

instance MonadIO m => DataGetter Time m where
    getData = do liftIO $ Data Time <$> getCurrentTime

instance Pretty UTCTime where
    pretty = text . formatTime defaultTimeLocale "%c"

defaultTimeFormatter = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Time <:> ": " <:> Msg
@

That's it! You can use any function inside - both pure as well as IO. If you use pure function, just return the value. If you will execute 'runBaseLogger' it will be evaluated inside the 'Identity' monad.

=== Custom logger transformers

It's also straightforward to define custom logger transformers. They have to be instances of some datatypes. To know more about it, look at example transformers inside the ''System.Log.Logger'' module.

= Conclusion

This is a new logging library written for purposes of fast logging between threads. It is still under development, so you can expect some api changes. There is still some functionality missing, like file handlers, but as you have seen, it is easy to define such. Any help would be welcome.

Happy logging!

-}

module System.Log (
    module X
) where

import System.Log.Simple as X

test = do
    addHandler $ addFilter (lvlFilter Warning) $ printHandler Nothing
    debug "a debug"
    warning "a warning"

main = do
    print =<< ( runBaseLoggerT (Lvl, Msg, Time) 
              . runHandlerLoggerT defaultTimeFormatter
              ) test
