{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Server.V1.Metrics where

import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Concurrent.MVar(MVar)
import qualified Control.Concurrent.MVar as MVar
  
import qualified System.Metrics.Prometheus.Http.Scrape as P
import           System.Metrics.Prometheus.Concurrent.RegistryT(RegistryT)
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as PR
import qualified System.Metrics.Prometheus.Metric.Counter as P

import           OpEnergy.Server.V1.Config
import           Data.OpEnergy.API.V1.Positive


-- | defines the whole state used by backend
data MetricsState = MetricsState
  { syncBlockHeadersCounter :: P.Counter
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> RegistryT m MetricsState
initMetrics _config = do
  syncBlockHeadersCounter <- PR.registerCounter "syncBlockHeader" mempty
  return $ MetricsState
    { syncBlockHeadersCounter = syncBlockHeadersCounter
    }

-- | runs metrics HTTP server
runMetricsServer :: Config -> MVar MetricsState -> IO ()
runMetricsServer config metricsV = do
  let Config{configPrometheusPort = metricsPort } = config
  PR.runRegistryT $ do
    metrics <- initMetrics config
    liftIO $ MVar.putMVar metricsV metrics
    P.serveMetricsT (fromPositive metricsPort) ["metrics"]
