{-- | This module is backend's entrypoint
 -}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
module Main where

import           Network.Wai.Handler.Warp
import           Data.Proxy
import qualified Data.Text.IO as Text
import           Servant
import           Control.Concurrent.Async
import qualified Control.Concurrent.MVar as MVar
import           System.IO
import           Control.Monad (forM, mapM)
import           Data.List as L
import           Control.Exception as E
import           Control.Monad.IO.Class( liftIO)
import           Control.Monad.Logger (runStdoutLoggingT, logInfo, askLoggerIO, LoggingT)
import           Prometheus(MonadMonitor(..))

import           Data.OpEnergy.API
import           OpEnergy.Server
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Metrics
import           OpEnergy.Server.V1.Class (State(..), defaultState, runAppT, runLogging)


instance MonadMonitor (LoggingT IO)

-- | entry point
main :: IO ()
main = runStdoutLoggingT $ do
  config <- liftIO $ OpEnergy.Server.V1.Config.getConfigFromEnvironment
  metricsV <- liftIO $ MVar.newEmptyMVar -- prometheus's thread will put value into this variable
  prometheusA <- liftIO $ asyncBound $ OpEnergy.Server.V1.Metrics.runMetricsServer config metricsV
  metrics <- liftIO $ MVar.readMVar metricsV
  state <- OpEnergy.Server.initState config metrics
  runAppT state $ runLogging $ $(logInfo) "bootstrap tasks"
  OpEnergy.Server.bootstrapTasks state
  -- now spawn worker threads
  schedulerA <- liftIO $ asyncBound $ runAppT state $ do -- this is scheduler thread, which goal is to perform periodical tasks
    runLogging $ $(logInfo) "scheduler thread"
    OpEnergy.Server.schedulerMainLoop
  serverA <- liftIO $ asyncBound $ runAppT state $ do -- this thread is for serving HTTP/websockets requests
    runLogging $ $(logInfo) "serving API"
    runServer
  liftIO $ waitAnyCancel $ -- waits for any of threads to shutdown in order to shutdown the rest
    [ serverA
    , schedulerA
    , prometheusA
    ]
  return ()
