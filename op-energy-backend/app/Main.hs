module Main where

import Network.Wai.Handler.Warp
import           Data.Proxy
import Servant
import Data.OpEnergy.API
import OpEnergy.Server.API
import OpEnergy.Server.V1.Config
import OpEnergy.Server.V1.DB
import OpEnergy.Server.V1
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
  hFlush stdout
  serverA <- asyncBound $ do
    print $ "serving API"
    hFlush stdout
    run (configHTTPAPIPort config) $ serve (Proxy :: Proxy API) $ OpEnergy.Server.API.server config pool
  waitAnyCancel $
    [ serverA
--    , schedulerA
--    , websocketsA
    ]
  return ()
