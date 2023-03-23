{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Server.V1 where

import           Data.Text                  (Text)
import           Servant
import           Control.Exception( Exception)
import qualified Control.Exception as E

import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Account
import           Data.OpEnergy.API.V1.Positive
import qualified OpEnergy.Server.GitCommitHash as Server
import           OpEnergy.Server.V1.Class (AppT)
import           OpEnergy.Server.V1.BlockHeadersService(syncBlockHeaders, getBlockHeaderByHash, getBlockHeaderByHeight)
import           OpEnergy.Server.V1.WebSocketService(webSocketConnection)
import           OpEnergy.Server.V1.BlockSpanService(getBlockSpanList)

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
    :<|> statisticsGet
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHash
    :<|> OpEnergy.Server.V1.BlockHeadersService.getBlockHeaderByHeight
    :<|> OpEnergy.Server.V1.BlockSpanService.getBlockSpanList
    :<|> oeGitHashGet

schedulerIteration :: AppT IO ()
schedulerIteration = OpEnergy.Server.V1.BlockHeadersService.syncBlockHeaders

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle hfoo payload = E.handle hfoo (payload >>= E.evaluate)

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

statisticsGet :: BlockHeight -> Text-> AppT Handler Statistics
statisticsGet = undefined

oeBlockSpanListGet :: BlockHeight-> Positive Int-> Positive Int-> AppT Handler [BlockSpan]
oeBlockSpanListGet = undefined

oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }
