{--
 - This module defines data type that keep all the metrics handlers
 -}
module OpEnergy.Server.V1.Metrics where

-- import           System.Clock (Clock(..), diffTimeSpec, getTime, toNanoSecs)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Concurrent.MVar(MVar)
import qualified Control.Concurrent.MVar as MVar
  
import qualified Prometheus as P
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus.Metric.GHC as P
import qualified Prometheus.Metric.Proc as P
import qualified Network.Wai.Handler.Warp as W

import           OpEnergy.Server.V1.Config
import           Data.OpEnergy.API.V1.Positive


-- | defines the whole state used by backend
data MetricsState = MetricsState
  { syncBlockHeadersH :: P.Histogram
  , btcGetBlockchainInfoH :: P.Histogram
  , mgetBlockHeaderByHeightH :: P.Histogram
  , mgetBlockHeaderByHashH :: P.Histogram
    -- for mgetBlockHeaderByHeight
  , mgetBlockHeaderByHeightCacheH :: P.Histogram
  , mgetBlockHeaderByHeightCacheHit :: P.Counter
  , mgetBlockHeaderByHeightCacheMiss :: P.Counter
  , mgetBlockHeaderByHeightCacheInsert :: P.Histogram
  , mgetBlockHeaderByHeightCacheDBLookup :: P.Histogram
    -- for getBlockHeaderByHash
  , mgetBlockHeaderByHashCacheH :: P.Histogram
  , mgetBlockHeaderByHashCacheHit :: P.Counter
  , mgetBlockHeaderByHashCacheMiss :: P.Counter
  , mgetBlockHeaderByHashCacheInsert :: P.Histogram
  , mgetBlockHeaderByHashCacheDBLookup :: P.Histogram
    -- for getBlockSpanList
  , getBlockSpanListH :: P.Histogram
    -- calculateStatistics
  , calculateStatisticsH :: P.Histogram
  , blockHeaderDBInsertH :: P.Histogram
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> m MetricsState
initMetrics _config = do
  syncBlockHeadersH <- P.register $ P.histogram (P.Info "syncBlockHeader" "") P.defaultBuckets
  btcGetBlockchainInfoH <- P.register $ P.histogram (P.Info "btcGetBlockchainInfo" "") P.defaultBuckets
  -- mgetBlockHeaderByHeight
  mgetBlockHeaderByHeightH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHeight" "") P.defaultBuckets
  mgetBlockHeaderByHeightCacheH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHeightCache" "") P.defaultBuckets
  mgetBlockHeaderByHeightCacheHit <- P.register $ P.counter (P.Info "mgetBlockHeaderByHeightCacheHit" "")
  mgetBlockHeaderByHeightCacheMiss <- P.register $ P.counter (P.Info "mgetBlockHeaderByHeightCacheMiss" "")
  mgetBlockHeaderByHeightCacheInsert <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHeightCacheInsert" "") P.defaultBuckets
  mgetBlockHeaderByHeightCacheDBLookup <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHeightDBLookup" "") P.defaultBuckets
  -- getBlockHeaderByHash
  mgetBlockHeaderByHashH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHash" "") P.defaultBuckets
  mgetBlockHeaderByHashCacheH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHashCache" "") P.defaultBuckets
  mgetBlockHeaderByHashCacheHit <- P.register $ P.counter (P.Info "mgetBlockHeaderByHashCacheHit" "")
  mgetBlockHeaderByHashCacheMiss <- P.register $ P.counter (P.Info "mgetBlockHeaderByHashCacheMiss" "")
  mgetBlockHeaderByHashCacheInsert <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHashCacheInsert" "") P.defaultBuckets
  mgetBlockHeaderByHashCacheDBLookup <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHashDBLookup" "") P.defaultBuckets
  -- getBockSpanList
  getBlockSpanListH <- P.register $ P.histogram (P.Info "getBlockSpanList" "") P.defaultBuckets
  calculateStatisticsH <- P.register $ P.histogram (P.Info "calculateStatistics" "") P.defaultBuckets
  blockHeaderDBInsertH <- P.register $ P.histogram (P.Info "blockHeaderDBInsertH" "") P.defaultBuckets
  _ <- P.register P.ghcMetrics
  _ <- P.register P.procMetrics
  return $ MetricsState
    { syncBlockHeadersH = syncBlockHeadersH
    , btcGetBlockchainInfoH = btcGetBlockchainInfoH
    , mgetBlockHeaderByHeightH = mgetBlockHeaderByHeightH
    , mgetBlockHeaderByHashH = mgetBlockHeaderByHashH
    , mgetBlockHeaderByHeightCacheH = mgetBlockHeaderByHeightCacheH
    , mgetBlockHeaderByHeightCacheHit = mgetBlockHeaderByHeightCacheHit
    , mgetBlockHeaderByHeightCacheMiss = mgetBlockHeaderByHeightCacheMiss
    , mgetBlockHeaderByHeightCacheInsert = mgetBlockHeaderByHeightCacheInsert
    , mgetBlockHeaderByHeightCacheDBLookup = mgetBlockHeaderByHeightCacheDBLookup
    , mgetBlockHeaderByHashCacheH = mgetBlockHeaderByHashCacheH
    , mgetBlockHeaderByHashCacheHit = mgetBlockHeaderByHashCacheHit
    , mgetBlockHeaderByHashCacheMiss = mgetBlockHeaderByHashCacheMiss
    , mgetBlockHeaderByHashCacheInsert = mgetBlockHeaderByHashCacheInsert
    , mgetBlockHeaderByHashCacheDBLookup = mgetBlockHeaderByHashCacheDBLookup
    , getBlockSpanListH = getBlockSpanListH
    , calculateStatisticsH = calculateStatisticsH
    , blockHeaderDBInsertH = blockHeaderDBInsertH
    }

-- | runs metrics HTTP server
runMetricsServer :: Config -> MVar MetricsState -> IO ()
runMetricsServer config metricsV = do
  let Config{configPrometheusPort = metricsPort } = config
  metrics <- initMetrics config
  MVar.putMVar metricsV metrics
  W.run (fromPositive metricsPort) P.metricsApp
