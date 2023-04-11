{--
 - This module defines data type that keep all the state, used by backend
 -}
module OpEnergy.Server.V1.Class where

import           Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Trans.Reader (runReaderT, ReaderT, ask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans(lift)
import           Control.Monad.Logger (runLoggingT, filterLogger, LoggingT, MonadLoggerIO, Loc, LogSource, LogLevel, LogStr)
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
  , logFunc :: LogFunc
  , logLevel :: TVar LogLevel
  }

type AppT = ReaderT State
type AppM = ReaderT State Handler

-- | constructs default state with given config and DB pool
defaultState :: (MonadLoggerIO m ) => Config-> LogFunc-> Pool SqlBackend-> m State
defaultState config logFunc _blockHeadersDBPool = do
  _blockHeadersHeightCache <- liftIO $ TVar.newTVarIO Map.empty
  _blockHeadersHashCache <- liftIO $ TVar.newTVarIO Map.empty
  _currentTip <- liftIO $ TVar.newTVarIO Nothing
  logLevelV <- liftIO $ TVar.newTVarIO (configLogLevelMin config)
  return $ State
    { config = config
    , blockHeadersHeightCache = _blockHeadersHeightCache
    , blockHeadersHashCache = _blockHeadersHashCache
    , blockHeadersDBPool = _blockHeadersDBPool
    , currentTip = _currentTip -- websockets' init data relies on whole BlockHeader
    , logFunc = logFunc
    , logLevel = logLevelV
    }

-- | Runs app transformer with given context
runAppT :: (Monad m) => State-> AppT m a-> m a
runAppT s x = runReaderT x s

runLogging :: MonadIO m => LoggingT m a -> AppT m ()
runLogging loggingAction = do
  State{ logFunc = logFunc, config = Config{ configLogLevelMin = logLevelMin}} <- ask
  let filterUnwantedLevels _source level = level >= logLevelMin
  _ <- lift $ runLoggingT (filterLogger filterUnwantedLevels loggingAction) logFunc
  return ()
