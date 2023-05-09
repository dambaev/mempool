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
  , btcGetBlockHashH :: P.Histogram
  , btcGetBlockH :: P.Histogram
  , btcGetBlockStatsH :: P.Histogram
  , mgetBlockHeaderByHeightH :: P.Histogram
  , mgetBlockHeaderByHashH :: P.Histogram
    -- for mgetBlockHeaderByHeight
  , blockHeaderHeightCacheH :: P.Histogram
  , blockHeaderHeightCacheHit :: P.Counter
  , blockHeaderHeightCacheMiss :: P.Counter
  , blockHeaderHeightCacheInsert :: P.Histogram
  , blockHeaderHeightCacheEnsureCapacity :: P.Histogram
    -- for getBlockHeaderByHash
  , blockHeaderHashCacheH :: P.Histogram
  , blockHeaderHashCacheHit :: P.Counter
  , blockHeaderHashCacheMiss :: P.Counter
  , blockHeaderHashCacheInsert :: P.Histogram
    -- for getBlockSpanList
  , getBlockSpanListH :: P.Histogram
    -- calculateStatistics
  , calculateStatisticsH :: P.Histogram
    -- insertion into DB table BlockHeader
  , blockHeaderDBInsertH :: P.Histogram
  , blockHeaderCacheFromDBLookup :: P.Histogram
  }

-- | constructs default state with given config and DB pool
initMetrics :: MonadIO m => Config-> m MetricsState
initMetrics _config = do
  syncBlockHeadersH <- P.register $ P.histogram (P.Info "syncBlockHeader" "") P.defaultBuckets
  btcGetBlockchainInfoH <- P.register $ P.histogram (P.Info "btcGetBlockchainInfo" "") P.defaultBuckets
  btcGetBlockHashH <- P.register $ P.histogram (P.Info "btcGetBlockHashH" "") P.defaultBuckets
  btcGetBlockH <- P.register $ P.histogram (P.Info "btcGetBlockH" "") P.defaultBuckets
  btcGetBlockStatsH <- P.register $ P.histogram (P.Info "btcGetBlockStatsH" "") P.defaultBuckets
  -- mgetBlockHeaderByHeight
  mgetBlockHeaderByHeightH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHeight" "") P.defaultBuckets
  blockHeaderHeightCacheH <- P.register $ P.histogram (P.Info "blockHeaderHeightCache" "") P.defaultBuckets
  blockHeaderHeightCacheHit <- P.register $ P.counter (P.Info "blockHeaderHeightCacheHit" "")
  blockHeaderHeightCacheMiss <- P.register $ P.counter (P.Info "blockHeaderHeightCacheMiss" "")
  blockHeaderHeightCacheInsert <- P.register $ P.histogram (P.Info "blockHeaderHeightCacheInsert" "") P.defaultBuckets
  -- getBlockHeaderByHash
  mgetBlockHeaderByHashH <- P.register $ P.histogram (P.Info "mgetBlockHeaderByHash" "") P.defaultBuckets
  blockHeaderHashCacheH <- P.register $ P.histogram (P.Info "blockHeaderHashCache" "") P.defaultBuckets
  blockHeaderHashCacheHit <- P.register $ P.counter (P.Info "blockHeaderHashCacheHit" "")
  blockHeaderHashCacheMiss <- P.register $ P.counter (P.Info "blockHeaderHashCacheMiss" "")
  blockHeaderHashCacheInsert <- P.register $ P.histogram (P.Info "blockHeaderHashCacheInsert" "") P.defaultBuckets
  -- getBockSpanList
  getBlockSpanListH <- P.register $ P.histogram (P.Info "getBlockSpanList" "") P.defaultBuckets
  calculateStatisticsH <- P.register $ P.histogram (P.Info "calculateStatistics" "") P.defaultBuckets
  blockHeaderDBInsertH <- P.register $ P.histogram (P.Info "blockHeaderDBInsert" "") P.defaultBuckets
  blockHeaderHeightCacheEnsureCapacity <- P.register $ P.histogram (P.Info "blockHeaderHeightCacheEnsureCapacity" "") P.defaultBuckets
  blockHeaderCacheFromDBLookup <- P.register $ P.histogram (P.Info "blockHeaderCacheFromDBLookup" "") P.defaultBuckets
  _ <- P.register P.ghcMetrics
  _ <- P.register P.procMetrics
  return $ MetricsState
    { syncBlockHeadersH = syncBlockHeadersH
    , btcGetBlockchainInfoH = btcGetBlockchainInfoH
    , btcGetBlockHashH = btcGetBlockHashH
    , btcGetBlockH = btcGetBlockH
    , btcGetBlockStatsH = btcGetBlockStatsH
    , mgetBlockHeaderByHeightH = mgetBlockHeaderByHeightH
    , mgetBlockHeaderByHashH = mgetBlockHeaderByHashH
    , blockHeaderHeightCacheH = blockHeaderHeightCacheH
    , blockHeaderHeightCacheHit = blockHeaderHeightCacheHit
    , blockHeaderHeightCacheMiss = blockHeaderHeightCacheMiss
    , blockHeaderHeightCacheInsert = blockHeaderHeightCacheInsert
    , blockHeaderHeightCacheEnsureCapacity = blockHeaderHeightCacheEnsureCapacity
    , blockHeaderHashCacheH = blockHeaderHashCacheH
    , blockHeaderHashCacheHit = blockHeaderHashCacheHit
    , blockHeaderHashCacheMiss = blockHeaderHashCacheMiss
    , blockHeaderHashCacheInsert = blockHeaderHashCacheInsert
    , getBlockSpanListH = getBlockSpanListH
    , calculateStatisticsH = calculateStatisticsH
    , blockHeaderDBInsertH = blockHeaderDBInsertH
    , blockHeaderCacheFromDBLookup = blockHeaderCacheFromDBLookup
    }

-- | runs metrics HTTP server
runMetricsServer :: Config -> MVar MetricsState -> IO ()
runMetricsServer config metricsV = do
  let Config{configPrometheusPort = metricsPort } = config
  metrics <- initMetrics config
  MVar.putMVar metricsV metrics
  W.run (fromPositive metricsPort) P.metricsApp
