module OpEnergy.Server.V1.StatisticsService where

import           Control.Monad (forM)
import           Control.Monad.Reader (ask)
import qualified Data.List as List

import Data.OpEnergy.API.V1.Block
import Data.OpEnergy.API.V1.Positive
import Data.OpEnergy.API.V1 (NbdrStatistics(..), Statistics(..))
import OpEnergy.Server.V1.Class
import OpEnergy.Server.V1.Config
import OpEnergy.Server.V1.BlockSpanService
import OpEnergy.Server.V1.BlockHeadersService

calculateStatistics :: BlockHeight -> Positive Int -> AppM Statistics
calculateStatistics startHeight span = do
  State{ config = Config { configStatisticsBlockSpansCount = statisticsBlockSpansCount}} <- ask
  blockSpans <- getBlockSpanList startHeight span $! positiveFromPositive2 statisticsBlockSpansCount
  let theoreticalBlockSpanTime = span * 600
      theoreticalBlockSpanTimePercent = theoreticalBlockSpanTime * 100
  discoverSpeeds::[Double] <- forM blockSpans $ \(BlockSpan start end) -> do
    startBlock <- getBlockHeaderByHeight start
    endBlock <- getBlockHeaderByHeight end
    return $! (fromIntegral theoreticalBlockSpanTimePercent) / ((fromIntegral (blockHeaderTimestamp endBlock)) - (fromIntegral (blockHeaderTimestamp startBlock)))
  let avg = (List.foldl' (\acc v -> acc + v) 0.0 discoverSpeeds ) / (fromIntegral statisticsBlockSpansCount)
      stddev = sqrt $! (List.foldl' (\acc i-> acc + (i - avg) ^ (2 :: Int)) 0.0 discoverSpeeds) / (fromIntegral ((unPositive2 statisticsBlockSpansCount) - 1))
  return $ Statistics
    { nbdr = NbdrStatistics
      { avg = avg
      , stddev = stddev
      }
    }
