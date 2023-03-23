
module OpEnergy.Server.V1.BlockSpanService where


import           Data.OpEnergy.API.V1.Block
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           OpEnergy.Server.V1.Class ( AppM)


getBlockSpanList :: BlockHeight-> Positive Int-> Positive Int-> AppM [BlockSpan]
getBlockSpanList startHeight span numberOfSpans = return spans
  where
    _span = fromPositive span
    _start = fromNatural startHeight
    spans = map (\i -> BlockSpan (verifyNatural (_start +  (i * _span))) (verifyNatural (_start + _span + i*_span))) [ 0 .. (fromPositive numberOfSpans) - 1]
