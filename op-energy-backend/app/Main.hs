module Main where

import           Network.Wai.Handler.Warp
import           Data.Proxy
import qualified Data.Text.IO as Text
import           Servant
import           Data.OpEnergy.API
import           OpEnergy.Server
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.DB
import           OpEnergy.Server.V1.Class (State(..), defaultState)
import           Control.Concurrent.Async
import           System.IO
import           Control.Monad (forM, mapM)
import           Data.List as L
import           Control.Exception as E

main :: IO ()
main = do
  config <- OpEnergy.Server.V1.Config.getConfigFromEnvironment
  print config
  pool <- OpEnergy.Server.V1.DB.getConnection config
  defState <- defaultState pool
  let state = defState
                { config = config
                }
  hFlush stdout
  Text.putStrLn "bootstrap tasks"
  OpEnergy.Server.bootstrapTasks state
  -- now spawn worker threads
  schedulerA <- asyncBound $ do
    Text.putStrLn "scheduler thread"
    hFlush stdout
    runAppT state OpEnergy.Server.schedulerMainLoop
  serverA <- asyncBound $ do
    Text.putStrLn "serving API"
    hFlush stdout
    runServer state
  waitAnyCancel $
    [ serverA
    , schedulerA
--    , websocketsA
    ]
  return ()
