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
import           Control.Concurrent
import           Control.Exception( Exception)
import qualified Control.Exception as E
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ask)

import           Data.OpEnergy.API.V1
import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Account
import           Data.OpEnergy.API.V1.Positive
import qualified OpEnergy.Server.GitCommitHash as Server
import           OpEnergy.Server.V1.Config
import           OpEnergy.Server.V1.Class (AppT, State(..))

-- | here goes implementation of OpEnergy API, which should match Data.OpEnergy.API.V1.V1API
server:: ServerT V1API (AppT Handler)
server = registerPost
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

schedulerMain :: AppT IO ()
schedulerMain = do
  State{ config = Config{ configSchedulerPollRateSecs = delaySecs }} <- ask
  liftIO $ putStrLn "scheduler main loop"
  liftIO $ threadDelay ((fromPositive delaySecs) * 1000000)
  schedulerMain

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

oeBlockGet :: BlockHash-> AppT Handler BlockHeader
oeBlockGet = undefined

oeBlockByHeightGet :: BlockHeight -> AppT Handler BlockHeader
oeBlockByHeightGet = undefined

oeBlockSpanListGet :: BlockHeight-> Positive Int-> Positive Int-> AppT Handler [BlockSpan]
oeBlockSpanListGet = undefined

oeGitHashGet :: AppT Handler GitHashResponse
oeGitHashGet = return $ GitHashResponse
  { gitCommitHash = Server.gitCommitHash
  }
