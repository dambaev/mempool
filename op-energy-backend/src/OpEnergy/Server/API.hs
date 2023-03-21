{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module OpEnergy.Server.API where

import           Servant

import           Data.OpEnergy.API
import           OpEnergy.Server.V1
import           OpEnergy.Server.V1.Config
import           Data.Pool(Pool)
import           Database.Persist.Postgresql

-- | Combined server of a Todo service with Swagger documentation.
server :: Config-> Pool SqlBackend-> Server API
server config conns = (return apiSwagger)
  :<|> OpEnergy.Server.V1.server config conns

