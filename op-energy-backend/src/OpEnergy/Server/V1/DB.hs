{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls  #-}


module OpEnergy.Server.V1.DB where

import           Data.Pool
import qualified Data.Text.Show as T
import qualified Data.Text.Encoding as TE
import            Control.Monad.Logger    (runStderrLoggingT)

import            Database.Persist.Postgresql

import            OpEnergy.Server.V1.Config
import            Data.OpEnergy.API.V1.Block
import            Data.OpEnergy.API.V1.Positive(fromPositive)

-- | connect to DB. Returns connection pool
getConnection :: Config -> IO (Pool SqlBackend)
getConnection config = do
  pool <- runStderrLoggingT $ createPostgresqlPool
    connStr
    (fromPositive $ configDBConnectionPoolSize config)
  flip runSqlPersistMPool pool $ runMigration migrateBlock -- perform necessary migrations. currently only BlockHeader table's migrations
  return pool
  where
    connStr = TE.encodeUtf8
      $! "host=" <> configDBHost config
      <> " port=" <> (T.tshow $ configDBPort config)
      <> " user=" <> configDBUser config
      <> " dbname=" <> configDBName config
      <> " password=" <> configDBPassword config
