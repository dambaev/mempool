{-- | This module is backend's entrypoint
 -}
module Main where

import           Network.Wai.Handler.Warp
import           Data.Proxy
import qualified Data.Text.IO as Text
import           Servant
import           Data.OpEnergy.API
import           OpEnergy.Server
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (State(..), defaultState, runAppT)
import           Control.Concurrent.Async
import           System.IO
import           Control.Monad (forM, mapM)
import           Data.List as L
import           Control.Exception as E


-- | entry point
main :: IO ()
main = do
  state <- OpEnergy.Server.initState
  Text.putStrLn "bootstrap tasks"
  OpEnergy.Server.bootstrapTasks state
  -- now spawn worker threads
  schedulerA <- asyncBound $ do -- this is scheduler thread, which goal is to perform periodical tasks
    Text.putStrLn "scheduler thread"
    hFlush stdout
    runAppT state OpEnergy.Server.schedulerMainLoop
  serverA <- asyncBound $ do -- this thread is for serving HTTP/websockets requests
    Text.putStrLn "serving API"
    hFlush stdout
    runServer state
  waitAnyCancel $ -- waits for any of threads to shutdown in order to shutdown the rest
    [ serverA
    , schedulerA
    ]
  return ()
