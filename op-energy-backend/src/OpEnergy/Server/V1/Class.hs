module OpEnergy.Server.V1.Class where


import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Servant (Handler)

import           Data.OpEnergy.API.V1.Block (BlockHash, BlockHeight, BlockHeader)
import           OpEnergy.Server.V1.Config
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend)

data State = State
  { config :: Config -- app config
  , blockHeadersDBPool :: Pool SqlBackend -- connection pool to BlockHeadersDB
  , blockHeadersHeightCache :: TVar (Map BlockHeight BlockHeader) -- BlockHeaders' cache: Height -> BlockHeader
  , blockHeadersHashCache :: TVar (Map BlockHash BlockHeight) -- BlockHeaders' cache: BlockHash -> BlockHeight
  , currentHeightTip :: TVar (Maybe BlockHeight) -- ^ defines the newest witnessed confirmed block height
  , currentTip :: TVar (Maybe BlockHeader) -- ^ defines the newest witnessed confirmed block
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

defaultState :: Pool SqlBackend-> IO State
defaultState _blockHeadersDBPool = do
  _blockHeadersHeightCache <- TVar.newTVarIO Map.empty
  _blockHeadersHashCache <- TVar.newTVarIO Map.empty
  _currentHeightTip <- TVar.newTVarIO Nothing
  _currentTip <- TVar.newTVarIO Nothing
  return $ State
    { config = defaultConfig
    , blockHeadersHeightCache = _blockHeadersHeightCache
    , blockHeadersHashCache = _blockHeadersHashCache
    , blockHeadersDBPool = _blockHeadersDBPool
    , currentHeightTip = _currentHeightTip
    , currentTip = _currentTip -- websockets' init data relies on whole BlockHeader
    }
