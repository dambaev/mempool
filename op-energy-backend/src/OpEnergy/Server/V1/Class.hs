{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.Server.V1.Class where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (runReaderT, ReaderT)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Servant (Handler)

import           Data.OpEnergy.API.V1.Block (BlockHash, BlockHeight, BlockHeader)
import           OpEnergy.Server.V1.Config
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend)

-- | defines the whole state used by backend
data State = State
  { config :: Config
  -- ^ app config, loaded from file
  , blockHeadersDBPool :: Pool SqlBackend
  -- ^ DB connection pool to BlockHeadersDB
  , blockHeadersHeightCache :: TVar (Map BlockHeight BlockHeader)
  -- ^ BlockHeaders' cache: BlockHeight -> BlockHeader. At the moment, cache is not expireable
  , blockHeadersHashCache :: TVar (Map BlockHash BlockHeight)
  -- ^ BlockHeaders' cache: BlockHash -> BlockHeight. At the moment, cache is not expireable
  , currentTip :: TVar (Maybe BlockHeader)
  -- ^ defines the newest witnessed confirmed block
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

-- | constructs default state with given config and DB pool
defaultState :: Config-> Pool SqlBackend-> IO State
defaultState config _blockHeadersDBPool = do
  _blockHeadersHeightCache <- TVar.newTVarIO Map.empty
  _blockHeadersHashCache <- TVar.newTVarIO Map.empty
  _currentTip <- TVar.newTVarIO Nothing
  return $ State
    { config = config
    , blockHeadersHeightCache = _blockHeadersHeightCache
    , blockHeadersHashCache = _blockHeadersHashCache
    , blockHeadersDBPool = _blockHeadersDBPool
    , currentTip = _currentTip -- websockets' init data relies on whole BlockHeader
    }

-- | Runs app transformer with given context
runAppT :: Monad m => State-> AppT m a-> m a
runAppT s x = runReaderT x s
