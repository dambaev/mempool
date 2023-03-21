{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
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
module Data.OpEnergy.API.V1.Block where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Data.Int
import           Data.Word

import           Data.OpEnergy.API.V1.Natural
import           Data.OpEnergy.API.V1.Hash
import           Database.Persist.TH
import           Database.Persist
import           Database.Persist.Sql
import           Numeric

share [mkPersist sqlSettings, mkMigrate "migrateBlock"] [persistLowerCase|
BlockHeader
  hash Hash
  previousblockhash Hash Maybe -- bitcoin/src/primitives/block.h uint256 hashPrevBlock;
  height (Natural Int)
  version Int32 -- bitcoin/src/primitives/block.h: int32_t nVersion;
  timestamp Word32 -- bitcoin/src/primitives/block.h uint32_t nTime
  bits Word32 -- bitcoin/src/primitives/block.h uint32_t nBits
  nonce Word32 -- bitcoin/src/primitives/block.h uint32_t nNonce;
  difficulty Double -- ./src/rpc/blockchain.h:double GetDifficulty(const CBlockIndex* blockindex);
  merkle_root Hash -- bitcoin/src/primitives/block.h uint256 hashMerkleRoot
  tx_count (Natural Int)
  size (Natural Int)
  weight Int64 -- src/consensus/validation.h:static inline int64_t GetBlockWeight(const CBlock& block)
  chainwork (Natural Integer) -- bitcoin/src/chain.h arith_uint256 nChainWork{}; (memory only) Total amount of work (expected number of hashes) in the chain up to and including this block
  mediantime Word32
  UniqueHash hash
  UniqueHeight height
  deriving Eq Show Generic
|]


defaultBlockHeader:: BlockHeader
defaultBlockHeader = BlockHeader
  { blockHeaderHash = verifyHash "0000000000000000000135d442ddb5ad7a8cdf92eb8496265d724804587bdf41"
  , blockHeaderPreviousblockhash = mverifyHash "00000000000000000004fd7d4e275888070a2c57fbbaa145d576f935f67645f8"
  , blockHeaderHeight = 772473
  , blockHeaderVersion = 538304512
  , blockHeaderTimestamp = 1674018057
  , blockHeaderBits = 386366690
  , blockHeaderNonce = 2589914493
  , blockHeaderDifficulty = 37590453655497.09
  , blockHeaderMerkle_root = verifyHash "847457eb7723bbe1e60a73ad6ff3016b630bf3595409eaa6a3f45e3cc1b54cf0"
  , blockHeaderTx_count = 2303
  , blockHeaderSize = 1528844
  , blockHeaderWeight = 3992705
  , blockHeaderChainwork = verifyNatural $ fst $ head $ readHex "00000000000000000000000000000000000000003dfd08c2b6932fc194a1fee4"
  , blockHeaderMediantime = 1674012509
  }

instance ToJSON BlockHeader
instance FromJSON BlockHeader
instance ToSchema BlockHeader where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockHeader schema"
    & mapped.schema.example ?~ toJSON defaultBlockHeader

type BlockHash = Hash

data BlockSpan = BlockSpan
  { startBlockHeight :: BlockHeight
  , endBlockHeight :: BlockHeight
  }
  deriving (Show, Generic, Typeable)

defaultBlockSpan :: BlockSpan
defaultBlockSpan = BlockSpan 772472 772473

instance ToJSON BlockSpan
instance FromJSON BlockSpan
instance ToSchema BlockSpan where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpan schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpan

type BlockHeight = Natural Int

defaultBlockHeight :: BlockHeight
defaultBlockHeight = verifyNaturalInt 1
  
