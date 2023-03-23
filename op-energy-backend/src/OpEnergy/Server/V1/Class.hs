module OpEnergy.Server.V1.Class where


import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Servant (Handler)

import           Data.OpEnergy.API.V1.Block (BlockHeight, BlockHeader)
import           OpEnergy.Server.V1.Config
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend)

data State = State
  { config :: Config -- app config
  , blockHeadersDBPool :: Pool SqlBackend -- connection pool to BlockHeadersDB
  , blockHeadersCache :: TVar (Map BlockHeight BlockHeader) -- BlockHeaders' cache
  , currentHeightTip :: TVar (Maybe BlockHeight) -- ^ defines the newest witnessed confirmed block height
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

defaultState :: Pool SqlBackend-> IO State
defaultState _blockHeadersDBPool = do
  _blockHeadersCache <- TVar.newTVarIO Map.empty
  _currentHeightTip <- TVar.newTVarIO Nothing
  return $ State
    { config = defaultConfig
    , blockHeadersCache = _blockHeadersCache
    , blockHeadersDBPool = _blockHeadersDBPool
    , currentHeightTip = _currentHeightTip
    }
