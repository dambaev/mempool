
module OpEnergy.Server.V1.WebSocketService where

import           System.IO as IO
import           Data.Text(Text)
import qualified Data.Text.IO as Text
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Concurrent (threadDelay)
import           Network.WebSockets (Connection, withPingThread, sendTextData)

-- import           Servant.API.WebSocket
import           OpEnergy.Server.V1.Class (AppM)


webSocketConnection :: Connection-> AppM ()
webSocketConnection conn = liftIO $ do
  Text.putStrLn "new websocket connection" >> IO.hFlush stdout
  withPingThread conn 10 (return ()) $ do
    forM_ [ (1::Int) .. ] $ \_ -> do
      sendTextData conn ("[]"::Text)
      threadDelay 10000000
