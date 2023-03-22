{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module OpEnergy.Server.API where

import           Servant
import           Network.Wai.Handler.Warp
import           Control.Monad.Trans.Reader (runReaderT)

import           Data.OpEnergy.API
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (AppT, AppM, State(..))


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
