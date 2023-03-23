{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module OpEnergy.Server where

import           Servant
import           Network.Wai.Handler.Warp
import           Control.Monad.Trans.Reader (runReaderT, ask)
import qualified Data.Text.IO as Text
import           Control.Concurrent
import           Control.Monad.IO.Class(liftIO)

import           Data.OpEnergy.API
import           Data.OpEnergy.API.V1.Positive
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (AppT, AppM, State(..))
import           OpEnergy.Server.V1.BlockHeadersService (loadDBState, syncBlockHeaders)


runAppT :: Monad m => State-> AppT m a-> m a
runAppT s x = runReaderT x s

-- | Runs HTTP server on a port defined in config in the State datatype
runServer :: State -> IO ()
runServer s = run port (app s)
  where
    port = configHTTPAPIPort (config s)
    app :: State-> Application
    app s = serve api $ hoistServer api (runAppT s) serverSwaggerBackend
      where
        api :: Proxy API
        api = Proxy
        -- | Combined server of a OpEnergy service with Swagger documentation.
        serverSwaggerBackend :: ServerT API AppM
        serverSwaggerBackend = (return apiSwagger)
          :<|> OpEnergy.Server.V1.server

bootstrapTasks :: State -> IO ()
bootstrapTasks s = do
  runAppT s $ do
    OpEnergy.Server.V1.BlockHeadersService.loadDBState -- first, load DB state
    OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders -- check for missing blocks

-- | main loop of the scheduler. Exception in this procedure will cause app to fail
schedulerMainLoop :: AppT IO ()
schedulerMainLoop = do
  State{ config = Config{ configSchedulerPollRateSecs = delaySecs }} <- ask
  liftIO $ Text.putStrLn "scheduler main loop"
  OpEnergy.Server.V1.schedulerIteration
  liftIO $ threadDelay ((fromPositive delaySecs) * 1000000)
  schedulerMainLoop
