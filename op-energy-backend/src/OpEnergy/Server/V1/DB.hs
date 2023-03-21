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
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import            Control.Monad.Logger    (runStderrLoggingT)

import            Database.Persist.Postgresql

import            OpEnergy.Server.V1.Config
import            Data.OpEnergy.API.V1.Block

tshow :: Show a => a -> Text
tshow some = T.pack $! show some

getConnection :: Config -> IO (Pool SqlBackend)
getConnection config = do
  pool <- runStderrLoggingT $ createPostgresqlPool
    connStr
    32
  flip runSqlPersistMPool pool $ runMigration migrateBlock
  return pool
  where
    connStr = TE.encodeUtf8
      $! "host=" <> configDBHost config
      <> " port=" <> (tshow $ configDBPort config)
      <> " user=" <> configDBUser config
      <> " dbname=" <> configDBName config
      <> " password=" <> configDBPassword config
