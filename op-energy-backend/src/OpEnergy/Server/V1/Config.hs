{-# LANGUAGE OverloadedStrings #-}
module OpEnergy.Server.V1.Config where

import           Data.Text (Text)
import           Data.Maybe
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified System.Environment as E
import           Servant.Client (BaseUrl(..), showBaseUrl, parseBaseUrl, Scheme(..))
import           Data.OpEnergy.API.V1.Positive
import           Data.OpEnergy.API.V1.Natural
import           Control.Monad.Catch

instance MonadThrow Parser where
  throwM = fail . show

data Config = Config
  { configDBPort :: Int
  , configDBHost:: Text
  , configDBUser :: Text
  , configDBName :: Text
  , configDBPassword :: Text
  , configSalt :: Text
  , configHTTPAPIPort :: Int
  , configBTCURL :: BaseUrl
  , configBTCUser :: Text
  , configBTCPassword :: Text
  , configBTCPollRateSecs :: Positive Int
  , configSchedulerPollRateSecs :: Positive Int
  , configBlocksToConfirm :: Natural Int
  }
  deriving Show
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v-> Config
    <$> ( v .:? "DB_PORT" .!= (configDBPort defaultConfig))
    <*> ( v .:? "DB_HOST" .!= (configDBHost defaultConfig))
    <*> ( v .:? "DB_USER" .!= (configDBUser defaultConfig))
    <*> ( v .:? "DB_NAME" .!= (configDBName defaultConfig))
    <*> ( v .:? "DB_PASSWORD" .!= (configDBPassword defaultConfig))
    <*> ( v .:? "SECRET_SALT" .!= (configSalt defaultConfig))
    <*> ( v .:? "API_HTTP_PORT" .!= (configHTTPAPIPort defaultConfig))
    <*> (( v .:? "BTC_URL" .!= (showBaseUrl $ configBTCURL defaultConfig)) >>= parseBaseUrl)
    <*> ( v .:? "BTC_USER" .!= (configBTCUser defaultConfig))
    <*> ( v .:? "BTC_PASSWORD" .!= (configBTCPassword defaultConfig))
    <*> ( v .:? "BTC_POLL_RATE_SECS" .!= (configBTCPollRateSecs defaultConfig))
    <*> ( v .:? "SCHEDULER_POLL_RATE_SECS" .!= (configSchedulerPollRateSecs defaultConfig))
    <*> ( v .:? "BLOCKS_TO_CONFIRM" .!= (configBlocksToConfirm defaultConfig))

defaultConfig:: Config
defaultConfig = Config
  { configDBPort = 5432
  , configDBHost = "localhost"
  , configDBUser = "openergy"
  , configDBName = "openergy"
  , configDBPassword = ""
  , configSalt = ""
  , configHTTPAPIPort = 8999
  , configBTCURL = BaseUrl Http "localhost" 8332 ""
  , configBTCUser = "user"
  , configBTCPassword = "password"
  , configBTCPollRateSecs = verifyPositive 1
  , configSchedulerPollRateSecs = verifyPositive 1
  , configBlocksToConfirm = 6
  }

getConfigFromEnvironment :: IO Config
getConfigFromEnvironment = do
  configFilePath <- E.lookupEnv "OPENERGY_BACKEND_CONFIG_FILE" >>= pure . fromMaybe "./op-energy-config.json"
  configStr <- BS.readFile configFilePath
  case A.eitherDecodeStrict configStr of
    Left some -> error $ configFilePath ++ " is not a valid config: " ++ some
    Right config -> return config
