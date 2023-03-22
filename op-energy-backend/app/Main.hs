module Main where

import Network.Wai.Handler.Warp
import           Data.Proxy
import Servant
import Data.OpEnergy.API
import OpEnergy.Server.API
import OpEnergy.Server.V1
import OpEnergy.Server.V1.Config
import OpEnergy.Server.V1.DB
import OpEnergy.Server.V1.Class (State(..), defaultState)
import Control.Concurrent.Async
import System.IO
import Control.Monad (forM, mapM)
import Data.List as L
import Control.Exception as E

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
  serverA <- asyncBound $ do
    print $ "serving API"
    hFlush stdout
    runServer state
  waitAnyCancel $
    [ serverA
--    , schedulerA
--    , websocketsA
    ]
  return ()
