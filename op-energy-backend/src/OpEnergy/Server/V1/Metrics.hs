{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Server.V1.Metrics where

import           System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Control.Concurrent.MVar(MVar)
import qualified Control.Concurrent.MVar as MVar
  
import qualified System.Metrics.Prometheus.Http.Scrape as P
import           System.Metrics.Prometheus.Concurrent.RegistryT(RegistryT)
import qualified System.Metrics.Prometheus.Concurrent.RegistryT as PR
import qualified System.Metrics.Prometheus.Metric.Histogram as P
import qualified System.Metrics.Prometheus.Metric.Gauge as P

import           OpEnergy.Server.V1.Config
import           Data.OpEnergy.API.V1.Positive


-- | defines the whole state used by backend
data MetricsState = MetricsState
  { syncBlockHeadersDuration :: P.Histogram
  , btcGetBlockchainInfoDuration :: P.Histogram
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> RegistryT m MetricsState
initMetrics _config = do
  syncBlockHeadersDuration <- PR.registerHistogram "syncBlockHeaderDuration" mempty []
  btcGetBlockchainInfoDuration <- PR.registerHistogram "btcGetBlockchainInfoDuration" mempty []
  return $ MetricsState
    { syncBlockHeadersDuration = syncBlockHeadersDuration
    , btcGetBlockchainInfoDuration = btcGetBlockchainInfoDuration
    }

-- | runs metrics HTTP server
runMetricsServer :: Config -> MVar MetricsState -> IO ()
runMetricsServer config metricsV = do
  let Config{configPrometheusPort = metricsPort } = config
  PR.runRegistryT $ do
    metrics <- initMetrics config
    liftIO $ MVar.putMVar metricsV metrics
    P.serveMetricsT (fromPositive metricsPort) ["metrics"]


observeDurationH :: MonadIO m => P.Histogram -> (m a)-> m a
observeDurationH h action = do
  start <- liftIO $ getTime Monotonic
  ret <- action
  end <- liftIO $ getTime Monotonic
  let duration = (fromIntegral (toNanoSecs (end `diffTimeSpec` start))) / 1000000000.0 -- convert to seconds
  liftIO $ P.observe duration h
  return ret

observeDurationG :: MonadIO m => P.Gauge -> (m a)-> m a
observeDurationG h action = do
  start <- liftIO $ getTime Monotonic
  ret <- action
  end <- liftIO $ getTime Monotonic
  let duration = (fromIntegral (toNanoSecs (end `diffTimeSpec` start))) / 1000000000.0 -- convert to seconds
  liftIO $ P.set duration h
  return ret
