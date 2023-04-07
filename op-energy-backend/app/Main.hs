{-- | This module is backend's entrypoint
 -}
{-# LANGUAGE TemplateHaskell            #-}
module Main where

import           Network.Wai.Handler.Warp
import           Data.Proxy
import qualified Data.Text.IO as Text
import           Servant
import           Control.Concurrent.Async
import           System.IO
import           Control.Monad (forM, mapM)
import           Data.List as L
import           Control.Exception as E
import           Control.Monad.IO.Class( liftIO)
import           Control.Monad.Logger (runStdoutLoggingT, logInfo, askLoggerIO)

import           Data.OpEnergy.API
import           OpEnergy.Server
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (State(..), defaultState, runAppT)


-- | entry point
main :: IO ()
main = runStdoutLoggingT $ do
  logFunc <- askLoggerIO
  state <- OpEnergy.Server.initState
  $(logInfo) "bootstrap tasks"
  OpEnergy.Server.bootstrapTasks state
  -- now spawn worker threads
  schedulerA <- liftIO $ asyncBound $ runAppT logFunc state $ do -- this is scheduler thread, which goal is to perform periodical tasks
    $(logInfo) "scheduler thread"
    OpEnergy.Server.schedulerMainLoop
  serverA <- liftIO $ asyncBound $ runAppT logFunc state $ do -- this thread is for serving HTTP/websockets requests
    $(logInfo) "serving API"
    runServer
  liftIO $ waitAnyCancel $ -- waits for any of threads to shutdown in order to shutdown the rest
    [ serverA
    , schedulerA
    ]
  return ()
