{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module OpEnergy.Server.V1 where

import           Data.Text                  (Text)
import           Data.Pool(Pool)
import           Servant
import           Control.Concurrent
import           Control.Exception( Exception)
import qualified Control.Exception as E



import Database.Persist.Postgresql as DB


import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Account
import           Data.OpEnergy.API.V1.Positive
import qualified OpEnergy.Server.GitCommitHash as Server
import           OpEnergy.Server.V1.Config

-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V1.V1API
server:: Config-> Pool SqlBackend -> Server V1API
server _ _
  = registerPost
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
  :<|> oeBlockGet 
  :<|> oeBlockByHeightGet 
  :<|> oeBlockSpanListGet 
  :<|> oeGitHashGet

schedulerMain :: Config -> Pool SqlBackend-> IO ()
schedulerMain config pool = do
  putStrLn "scheduler main loop"
  threadDelay ((fromPositive (configSchedulerPollRateSecs config)) * 1000000)
  schedulerMain config pool

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle hfoo payload = E.handle hfoo (payload >>= E.evaluate)

registerPost :: Handler RegisterResult
registerPost = undefined

loginPost :: [AccountSecret]-> Handler [AccountToken]
loginPost = undefined

strikeMediantimeGet :: Handler [TimeStrike]
strikeMediantimeGet = undefined

strikeBlockMediantimeGet :: BlockHeight-> Handler [TimeStrike]
strikeBlockMediantimeGet = undefined

strikeMediantimePost :: CreateTimeStrikeRequest -> Handler TimeStrike
strikeMediantimePost = undefined

slowFastGuessMediantimeGet :: BlockHeight-> NLockTime-> Handler [SlowFastGuess]
slowFastGuessMediantimeGet = undefined

slowFastGuessMediantimePost :: CreateSlowFastGuessRequest -> Handler SlowFastGuess
slowFastGuessMediantimePost = undefined

strikeHistoryMediantimeGet :: Handler [TimeStrikeHistory]
strikeHistoryMediantimeGet = undefined

slowFastResultsMediantimeGet :: [AccountToken]-> NLockTime-> BlockHeight-> Handler [SlowFastResult]
slowFastResultsMediantimeGet = undefined

userDisplayNamePost :: PostUserDisplayNameRequest -> Handler ()
userDisplayNamePost = undefined

statisticsGet :: BlockHeight -> Text-> Handler Statistics
statisticsGet = undefined

oeBlockGet :: BlockHash-> Handler BlockHeader
oeBlockGet = undefined

oeBlockByHeightGet :: BlockHeight -> Handler BlockHeader
oeBlockByHeightGet = undefined

oeBlockSpanListGet :: BlockHeight-> Positive Int-> Positive Int-> Handler [BlockSpan]
oeBlockSpanListGet = undefined

oeGitHashGet :: Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }
