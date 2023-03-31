{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V1.Positive where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Scientific( Scientific, toBoundedInteger)
import           Servant.API
import qualified Data.Text.Read as TR

newtype Positive a = Positive Int
  deriving (Ord, Real, Enum, Integral, Show, Generic, Typeable, Eq)

instance Num (Positive a) where
  (+) (Positive left) (Positive right) = verifyPositiveInt $! left + right
  (-) (Positive left) (Positive right) = verifyPositiveInt $! left - right
  (*) (Positive left) (Positive right) = verifyPositiveInt $! left * right
  abs v = v
  signum (Positive left) = verifyPositiveInt $ signum left
  fromInteger v = verifyPositiveInt (fromInteger v)

instance ToJSON (Positive Int)
instance FromJSON (Positive Int) where
  parseJSON = withScientific "Positive" $ \v-> return (verifyPositive v)
instance ToSchema (Positive Int) where
  declareNamedSchema _ = return $ NamedSchema (Just "Positive") $ mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 1
instance ToParamSchema (Positive Int) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 1
instance FromHttpApiData (Positive Int) where
  parseUrlPiece t =
    case TR.decimal t of
      Left _ -> Left "Positive: wrong value"
      Right (v, _) -> Right (verifyPositiveInt v)
  parseQueryParam t =
    case TR.decimal t of
      Left _ -> Left "Positive: wrong value"
      Right (v, _)-> Right (verifyPositiveInt v)
instance ToHttpApiData (Positive Int) where
  toUrlPiece (Positive v) = toUrlPiece v
  toQueryParam (Positive v) = toQueryParam v

verifyPositiveInt:: Int -> Positive a
verifyPositiveInt v =
  if v > 0
  then Positive v
  else error "verifyPositiveInt: wrong value"

verifyPositive:: Scientific -> Positive a
verifyPositive s =
  case toBoundedInteger s of
    Just v -> verifyPositiveInt v
    _  -> error "verifyPositive: wrong value"

fromPositive :: Positive a -> Int
fromPositive (Positive v) = v
