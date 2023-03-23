{-# LANGUAGE ScopedTypeVariables #-}
module OpEnergy.Server.V1.WebSocketService where

import           Data.Aeson as Aeson
import           System.IO as IO
import           Data.Text(Text)
import qualified Data.Text.IO as Text
import           Control.Monad ( forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Control.Concurrent.STM.TVar as TVar
import           Network.WebSockets (DataMessage(..), WebSocketsData(..), Connection, receiveData, withPingThread, sendTextData)

-- import           Servant.API.WebSocket
import           OpEnergy.Server.V1.Class (AppT, AppM, State(..))
import           Data.OpEnergy.API.V1.Block( BlockHeader)


data WebsocketRequest
  = ActionInit
  | ActionWant [Text]
  deriving (Eq, Show)

instance FromJSON WebsocketRequest where
  parseJSON = withObject "WebsocketRequest" $ \v-> do
    action::Text <- v .: "action"
    case action of
      "init" -> return ActionInit
      "want" -> ActionWant <$> v .: "data"
      _ -> return ActionInit
instance ToJSON WebsocketRequest where
  toJSON ActionInit = object [ "action" .= ("init" :: Text)]
  toJSON (ActionWant topics) = object
    [ "action" .= ("want" :: Text)
    , "data" .= topics
    ]
instance WebSocketsData WebsocketRequest where
  fromLazyByteString lbs =
    case Aeson.decode lbs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data"
  toLazyByteString action = Aeson.encode action
  fromDataMessage (Text bs _) =
    case Aeson.decode bs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data message"
  fromDataMessage (Binary bs) =
    case Aeson.decode bs of
      Just ret -> ret
      _ -> error "failed to parse Action from websockets data message"

-- | TODO: compatibility with mempool's frontend
data MempoolInfo = MempoolInfo
  { newestConfirmedBlock :: Maybe BlockHeader -- TODO: do we actually need BlockHeader here?
  }
  deriving (Show)
instance ToJSON MempoolInfo where
  toJSON mpi = object
    [ "mempoolInfo" .= object
      [ "oe-newest-confirmed-block" .= newestConfirmedBlock mpi
      ]
    ]

webSocketConnection :: Connection-> AppM ()
webSocketConnection conn = do
  state <- ask
  liftIO $ Text.putStrLn "new websocket connection" >> IO.hFlush stdout
  liftIO $ withPingThread conn 10 (sendTextData conn ("{\"pong\": true}" :: Text)) $ forever $ flip runReaderT state $ do
    req <- liftIO $ receiveData conn
    case req of
      ActionWant topics -> sendTopics topics
      ActionInit -> do
        mpi <- getMempoolInfo
        liftIO $ sendTextData conn $ Aeson.encode mpi
  where
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
