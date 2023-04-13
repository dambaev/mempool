{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances          #-}
module Data.OpEnergy.API.V1.Hash where

import           Data.Text                  (Text)
import qualified Data.Text as               T
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Swagger
import           Control.Lens
import           Servant.API
import           Data.Char (isAlphaNum)
import           Crypto.Hash.SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString as BS
import           Data.Word (Word8)
import qualified Data.Text.Encoding as TE
import           Control.Monad (replicateM)
import           System.Random

import           Database.Persist
import           Database.Persist.Sql

data Hash = Hash Text
  deriving (Show, Generic, Typeable, Eq, Ord)
instance ToJSON Hash
instance FromJSON Hash
instance ToParamSchema Hash where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"
instance ToHttpApiData Hash where
  toUrlPiece (Hash t) = toUrlPiece t
  toQueryParam (Hash t) = toQueryParam t
instance ToSchema Hash where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Hash schema"
    & mapped.schema.example ?~ toJSON ((Hash "b8ab3013e4ddb35fae6cedc9d84c86fd280157b7a93b984c0b40baf7f21b8f72") )
instance FromHttpApiData Hash where
  parseUrlPiece t = Right (verifyHash t)
  parseQueryParam t = Right (verifyHash t)

instance PersistField Hash where
  toPersistValue (Hash s) = toPersistValue s
  fromPersistValue (PersistText s) = Right $! verifyHash s -- TODO
  fromPersistValue _ = Left $ "InputVerification.hs fromPersistValue Hash , expected Text"
instance PersistFieldSql Hash where
  sqlType _ = SqlString


defaultHash :: Hash
defaultHash = Hash "b8ab3013e4adb35fae6cbdc9d84c86cd280157b7a93b984c0b40baf7f21b8f72"

generateRandomHash :: IO Hash
generateRandomHash = do
  rndBS <- (replicateM 10 $ getStdRandom (randomR (0::Word8, 255::Word8))) >>= return . BS.pack
  let base16 = Base16.encode $! hash rndBS
  return $! Hash $! TE.decodeUtf8 base16

everifyHash:: Text-> Either Text Hash
everifyHash raw =
  case () of
    _ | T.length limitedSize /= 64 -> Left "Hash: wrong size"
    _ | not (T.all isAlphaNum limitedSize ) -> Left "Hash: should be alpha num"
    _ -> Right (Hash limitedSize)
  where
    limitedSize = T.copy $! T.take 64 raw

mverifyHash:: Text-> Maybe Hash
mverifyHash raw =
  case everifyHash raw of
    Left _ -> Nothing
    Right ret -> Just ret

verifyHash:: Text-> Hash
verifyHash raw =
  case everifyHash raw of
    Right ret -> ret
    Left some -> error (show some)
