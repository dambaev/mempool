{-- |
 - This module is the top module of backend V1
 -}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Server.V1
  ( server
  , schedulerIteration
  )where

import           Servant
import           Control.Monad.IO.Class(MonadIO)

import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Account
import qualified OpEnergy.Server.GitCommitHash as Server
import           OpEnergy.Server.V1.Class (AppT)
import           OpEnergy.Server.V1.BlockHeadersService(syncBlockHeaders, getBlockHeaderByHash, getBlockHeaderByHeight)
import           OpEnergy.Server.V1.WebSocketService(webSocketConnection)
import           OpEnergy.Server.V1.BlockSpanService(getBlockSpanList)
import           OpEnergy.Server.V1.StatisticsService(calculateStatistics)

import           Prometheus(MonadMonitor)


-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V1.V1API
server:: ServerT V1API (AppT Handler)
server = OpEnergy.Server.V1.WebSocketService.webSocketConnection
    :<|> registerPost
    :<|> loginPost
    :<|> strikeMediantimeGet
    :<|> strikeBlockMediantimeGet
    :<|> strikeMediantimePost
    :<|> slowFastGuessMediantimeGet
    :<|> slowFastGuessMediantimePost
    :<|> strikeHistoryMediantimeGet
    :<|> slowFastResultsMediantimeGet
    :<|> userDisplayNamePost
    :<|> OpEnergy.Server.V1.StatisticsService.calculateStatistics
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHash
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHeight
    :<|> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList
    :<|> oeGitHashGet

-- | one iteration that called from scheduler thread
schedulerIteration :: (MonadIO m, MonadMonitor m) => AppT m ()
schedulerIteration = OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders

-- returns just commit hash, provided by build system
oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }

{- here goes a set of unimplemented yet handlers --> -}
registerPost :: AppT Handler RegisterResult
registerPost = undefined

loginPost :: [AccountSecret]-> AppT Handler [AccountToken]
loginPost = undefined

strikeMediantimeGet :: AppT Handler [TimeStrike]
strikeMediantimeGet = undefined

strikeBlockMediantimeGet :: BlockHeight-> AppT Handler [TimeStrike]
strikeBlockMediantimeGet = undefined

strikeMediantimePost :: CreateTimeStrikeRequest -> AppT Handler TimeStrike
strikeMediantimePost = undefined

slowFastGuessMediantimeGet :: BlockHeight-> NLockTime-> AppT Handler [SlowFastGuess]
slowFastGuessMediantimeGet = undefined

slowFastGuessMediantimePost :: CreateSlowFastGuessRequest -> AppT Handler SlowFastGuess
slowFastGuessMediantimePost = undefined

strikeHistoryMediantimeGet :: AppT Handler [TimeStrikeHistory]
strikeHistoryMediantimeGet = undefined

slowFastResultsMediantimeGet :: [AccountToken]-> NLockTime-> BlockHeight-> AppT Handler [SlowFastResult]
slowFastResultsMediantimeGet = undefined

userDisplayNamePost :: PostUserDisplayNameRequest -> AppT Handler ()
userDisplayNamePost = undefined
