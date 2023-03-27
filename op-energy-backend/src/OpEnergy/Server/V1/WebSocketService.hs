{-# LANGUAGE ScopedTypeVariables #-}
module OpEnergy.Server.V1.WebSocketService where

import           Data.Aeson as Aeson
import           System.IO as IO
import           Data.Text(Text)
import qualified Data.Text.IO as Text
import           Control.Exception as E
import           Control.Monad ( forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import           Data.IORef
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TVar as TVar
import           Network.WebSockets ( Connection, receiveData, withPingThread, sendTextData)
import           OpEnergy.Server.V1.DB (tshow)

-- import           Servant.API.WebSocket
import           OpEnergy.Server.V1.Class (AppT, AppM, State(..))
import           Data.OpEnergy.API.V1.Hash( Hash, generateRandomHash)
import           Data.OpEnergy.API.V1.Block( BlockHeight, BlockHeader(..))
import           OpEnergy.Server.V1.WebSocketService.Message



webSocketConnection :: Connection-> AppM ()
webSocketConnection conn = do
  state <- ask
  liftIO $ bracket (initConnection state) (closeConnection state) $ \(uuid, witnessedHeightV)-> do
    timeoutCounterV <- newIORef (10:: Int) -- used to determine if ping packet should be sent
    liftIO $ withPingThread conn 1 (checkIteration state uuid witnessedHeightV timeoutCounterV) $ forever $ flip runReaderT state $ do
      req <- liftIO $ receiveData conn
      case req of
        ActionWant topics -> sendTopics topics
        ActionInit -> do
          liftIO $ Text.putStrLn (tshow uuid <> " init data request")
          mpi <- getMempoolInfo
          liftIO $ sendTextData conn $ Aeson.encode mpi
          writeIORef timeoutCounterV 10 -- TODO: make it configurable
  where
    checkIteration state _ witnessedHeightV timeoutCounterV = do
      let State{ currentTip = currentTipV } = state
      mwitnessedHeight <- readIORef witnessedHeightV
      mcurrentTip <- STM.atomically $ TVar.readTVar currentTipV
      case (mwitnessedHeight, mcurrentTip) of
        ( _, Nothing) -> do -- haven't witnessed current tip and there is no tip loaded yet. decrease timeout
          decreaseTimeoutOrSendPing
        (Nothing, Just currentTip) -> do -- current connection saw no current tip yet: need to send one to the client and store
          sendTextData conn $ MessageNewestBlockHeader currentTip
          writeIORef timeoutCounterV 10 -- TODO: make it configurable
          writeIORef witnessedHeightV $! Just $! blockHeaderHeight currentTip
        ( Just witnessedHeight, Just currentTip)
          | witnessedHeight == blockHeaderHeight currentTip -> decreaseTimeoutOrSendPing -- current tip had already been witnessed
          | otherwise -> do
              sendTextData conn $ MessageNewestBlockHeader currentTip
              writeIORef timeoutCounterV 10 -- TODO: make it configurable
              writeIORef witnessedHeightV $! Just $! blockHeaderHeight currentTip
      where
        decreaseTimeoutOrSendPing :: IO ()
        decreaseTimeoutOrSendPing = do
          counter <- readIORef timeoutCounterV
          if counter == 0
            then do
              writeIORef timeoutCounterV 10 -- TODO: make it configurable
              sendTextData conn ("{\"pong\": true}" :: Text)
            else writeIORef timeoutCounterV (pred counter) -- decrease counter

    initConnection :: State-> IO (Hash, IORef (Maybe BlockHeight))
    initConnection state = do
      let State{websocketsBroadcastChan = _chan} = state
      witnessedTipV <- newIORef Nothing
      uuid <- generateRandomHash
      liftIO $ Text.putStrLn (tshow uuid <> ": new websocket connection") >> IO.hFlush stdout
      return (uuid, witnessedTipV)
    closeConnection _ (uuid, _) = do
      liftIO $ Text.putStrLn (tshow uuid <> ": closed websocket connection") >> IO.hFlush stdout
      return ()
    sendTopics [] = return ()
    sendTopics ("generatedaccounttoken" : rest) = do
      liftIO $ sendTextData conn ("{\"generatedAccountSecret\" : \"test\", \"generatedAccountToken\": \"test\"}"::Text)
      sendTopics rest
    sendTopics ( topic : rest) = do
      liftIO $ Text.putStrLn ("received unsupported ActionWant " <> topic) >> IO.hFlush stdout
      sendTopics rest

getMempoolInfo :: AppT IO MempoolInfo
getMempoolInfo = do
  State{ currentTip = currentTipV} <- ask
  mtip <- liftIO $ TVar.readTVarIO currentTipV
  return $ MempoolInfo
    { newestConfirmedBlock = mtip
    }
