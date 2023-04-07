{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.Server.V1.Class where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (runReaderT, ReaderT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runLoggingT, LoggingT, MonadLoggerIO, Loc, LogSource, LogLevel, LogStr)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Servant (Handler)

import           Data.OpEnergy.API.V1.Block (BlockHash, BlockHeight, BlockHeader)
import           OpEnergy.Server.V1.Config
import           Data.Pool(Pool)
import           Database.Persist.Postgresql (SqlBackend)

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

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

type AppT m = ReaderT State (LoggingT m)
type AppM = ReaderT State ( LoggingT Handler)

-- | constructs default state with given config and DB pool
defaultState :: (MonadLoggerIO m ) => Config-> Pool SqlBackend-> m State
defaultState config _blockHeadersDBPool = do
  _blockHeadersHeightCache <- liftIO $ TVar.newTVarIO Map.empty
  _blockHeadersHashCache <- liftIO $ TVar.newTVarIO Map.empty
  _currentTip <- liftIO $ TVar.newTVarIO Nothing
  return $ State
    { config = config
    , blockHeadersHeightCache = _blockHeadersHeightCache
    , blockHeadersHashCache = _blockHeadersHashCache
    , blockHeadersDBPool = _blockHeadersDBPool
    , currentTip = _currentTip -- websockets' init data relies on whole BlockHeader
    }

-- | Runs app transformer with given context
runAppT :: (Monad m) => LogFunc -> State-> AppT m a-> m a
runAppT logFunc s x = runLoggingT (runReaderT x s) logFunc

