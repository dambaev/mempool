{--| This module defines statistics routine
 -}
module OpEnergy.Server.V1.StatisticsService where

import           Control.Monad (forM)
import           Control.Monad.Reader (ask)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.List as List
import           Data.Maybe(fromJust)

import Data.OpEnergy.API.V1.Block
import Data.OpEnergy.API.V1.Positive
import Data.OpEnergy.API.V1 (NbdrStatistics(..), Statistics(..))
import OpEnergy.Server.V1.Class
import OpEnergy.Server.V1.Config
import OpEnergy.Server.V1.BlockSpanService
import OpEnergy.Server.V1.BlockHeadersService

-- | This function returns an average and std dev of blocks' discover speed in compare to theoretical discover speed of 10 minites (600 seconds),
-- which shows how fast blocks had been discovered: if it less than 100% - then blocks in block span are being discovered slower than theoretical speed. If it more than 100% - faster.
-- current implementation will try to calculate statistics for 'configStatisticsBlockSpansCount' spans of size 'span' starting from 'startHeight' block
calculateStatistics
  :: MonadIO m
  => BlockHeight
  -- ^ starting block height of a range for which statistics will be generated
  -> Positive Int
  -- ^ size of block span
  -> AppT m Statistics
calculateStatistics startHeight span = do
  State{ config = Config { configStatisticsBlockSpansCount = statisticsBlockSpansCount}} <- ask
  blockSpans <- getBlockSpanList startHeight span $! positiveFromPositive2 statisticsBlockSpansCount
  let theoreticalBlockSpanTime = span * 600
      theoreticalBlockSpanTimePercent = theoreticalBlockSpanTime * 100
  discoverSpeeds::[Double] <- forM blockSpans $ \(BlockSpan start end) -> do
    startBlock <- mgetBlockHeaderByHeight start >>= pure . fromJust
    endBlock <- mgetBlockHeaderByHeight end >>= pure . fromJust
    return $! (fromIntegral theoreticalBlockSpanTimePercent) / ((fromIntegral (blockHeaderMediantime endBlock)) - (fromIntegral (blockHeaderMediantime startBlock)))
  let avg = (List.foldl' (\acc v -> acc + v) 0.0 discoverSpeeds ) / (fromIntegral statisticsBlockSpansCount)
      stddev = sqrt $! (List.foldl' (\acc i-> acc + (i - avg) ^ (2 :: Int)) 0.0 discoverSpeeds) / (fromIntegral ((unPositive2 statisticsBlockSpansCount) - 1))
  return $ Statistics
    { nbdr = NbdrStatistics
      { avg = avg
      , stddev = stddev
      }
    }
