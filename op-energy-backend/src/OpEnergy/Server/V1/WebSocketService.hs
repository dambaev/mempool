{-# LANGUAGE ScopedTypeVariables #-}
module OpEnergy.Server.V1.WebSocketService where

import           Data.Aeson as Aeson
import           System.IO as IO
import           Data.Text(Text)
import qualified Data.Text.IO as Text
import           Data.Text.Show (tshow)
import           Control.Exception as E
import           Control.Monad ( forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import           Data.IORef
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Network.WebSockets ( Connection, receiveData, withPingThread, sendTextData)

import           OpEnergy.Server.V1.Class (AppT, AppM, State(..))
import           Data.OpEnergy.API.V1.Hash( Hash, generateRandomHash)
import           Data.OpEnergy.API.V1.Block( BlockHeight, BlockHeader(..))
import           Data.OpEnergy.API.V1.Positive(naturalFromPositive)
import           OpEnergy.Server.V1.WebSocketService.Message
import           OpEnergy.Server.V1.Config



-- | This procedure is an mainloop for every websocket connection, which:
-- - handles requests from clients;
-- - sends notification about newest confirmed block
-- - sends keepalive packets
webSocketConnection :: Connection-> AppM ()
webSocketConnection conn = do
  state <- ask
  liftIO $ bracket (initConnection state) (closeConnection state) $ \(uuid, witnessedHeightV)-> do
    let State{ config = Config { configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
    timeoutCounterV <- newIORef (naturalFromPositive configWebsocketKeepAliveSecs)
    liftIO $ withPingThread conn 1 (checkIteration state uuid witnessedHeightV timeoutCounterV) $ forever $ flip runReaderT state $ do
      req <- liftIO $ receiveData conn
      case req of
        ActionWant topics -> sendTopics topics -- handle requested topics
        ActionPing -> do
          liftIO $ do
            sendTextData conn MessagePong
            writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
        ActionInit -> do
          liftIO $ Text.putStrLn (tshow uuid <> " init data request")
          mpi <- getMempoolInfo
          liftIO $ do
            sendTextData conn $ Aeson.encode mpi
            writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs )
  where
    checkIteration state _ witnessedHeightV timeoutCounterV = do
      let State{ currentTip = currentTipV, config = Config {configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
      mwitnessedHeight <- readIORef witnessedHeightV
      mcurrentTip <- STM.atomically $ TVar.readTVar currentTipV
      case (mwitnessedHeight, mcurrentTip) of
        ( _, Nothing) -> do -- haven't witnessed current tip and there is no tip loaded yet. decrease timeout
          decreaseTimeoutOrSendPing state
        (Nothing, Just currentTip) -> do -- current connection saw no current tip yet: need to send one to the client and store
          sendTextData conn $ MessageNewestBlockHeader currentTip
          writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
          writeIORef witnessedHeightV $! Just $! blockHeaderHeight currentTip
        ( Just witnessedHeight, Just currentTip)
          | witnessedHeight == blockHeaderHeight currentTip -> decreaseTimeoutOrSendPing state -- current tip had already been witnessed
          | otherwise -> do -- notify client about newest confirmed block
              sendTextData conn $ MessageNewestBlockHeader currentTip
              writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
              writeIORef witnessedHeightV $! Just $! blockHeaderHeight currentTip
      where
        decreaseTimeoutOrSendPing :: State-> IO ()
        decreaseTimeoutOrSendPing state = do
          let State{ config = Config { configWebsocketKeepAliveSecs = configWebsocketKeepAliveSecs} } = state
          counter <- readIORef timeoutCounterV
          if counter == 0
            then do
              writeIORef timeoutCounterV (naturalFromPositive configWebsocketKeepAliveSecs)
              sendTextData conn ("{\"pong\": true}" :: Text)
            else writeIORef timeoutCounterV (pred counter) -- decrease counter

    initConnection :: State-> IO (Hash, IORef (Maybe BlockHeight))
    initConnection _state = do
      witnessedTipV <- newIORef Nothing
      uuid <- generateRandomHash
      liftIO $ Text.putStrLn (tshow uuid <> ": new websocket connection") >> IO.hFlush stdout
      return (uuid, witnessedTipV)
    closeConnection _ (uuid, _) = do
      liftIO $ Text.putStrLn (tshow uuid <> ": closed websocket connection") >> IO.hFlush stdout
      return ()
    sendTopics [] = return ()
    sendTopics ("generatedaccounttoken" : rest) = do -- for now send dummy secret/token
      liftIO $ sendTextData conn ("{\"generatedAccountSecret\" : \"test\", \"generatedAccountToken\": \"test\"}"::Text)
      sendTopics rest
    sendTopics ( topic : rest) = do
      liftIO $ Text.putStrLn ("received unsupported ActionWant " <> topic) >> IO.hFlush stdout
      sendTopics rest

-- currently, frontend expects 'mempool info' initial message from backend. This function provides such info
getMempoolInfo :: AppT IO MempoolInfo
getMempoolInfo = do
  State{ currentTip = currentTipV} <- ask
  mtip <- liftIO $ TVar.readTVarIO currentTipV
  return $ MempoolInfo
    { newestConfirmedBlock = mtip
    }
