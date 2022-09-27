module SidechainParams where

import Contract.Prelude

import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Types (PubKey)
import Types.Transaction (TransactionInput)

newtype SidechainParams = SidechainParams
  { chainId ∷ BigInt
  , genesisHash ∷ ByteArray
  , genesisMint ∷ Maybe TransactionInput
  , genesisUtxo ∷ TransactionInput
  }

derive instance Generic SidechainParams _
derive instance Newtype SidechainParams _
instance ToData SidechainParams where
  toData (SidechainParams { chainId, genesisHash, genesisMint, genesisUtxo }) =
    Constr zero
      [ toData chainId
      , toData genesisHash
      , toData genesisMint
      , toData genesisUtxo
      ]

instance Show SidechainParams where
  show = genericShow

-- | Parameters to initialize a sidechain
newtype InitSidechainParams = InitSidechainParams
  { initChainId ∷ BigInt
  , initGenesisHash ∷ ByteArray
  , -- | 'initUtxo ' is a 'TxOutRef' used for creating 'AssetClass's for the
    -- internal function of the side chain (e.g. InitCommitteeHashMint TODO: hyperlink this documentation)
    initUtxo ∷ TransactionInput
  , -- | 'initCommittee' is the initial committee of the sidechain
    initCommittee ∷ Array PubKey
  , initMint ∷ Maybe TransactionInput
  }

derive instance Generic InitSidechainParams _
derive instance Newtype InitSidechainParams _
instance ToData InitSidechainParams where
  toData
    ( InitSidechainParams
        { initChainId, initGenesisHash, initUtxo, initCommittee, initMint }
    ) =
    Constr zero
      [ toData initChainId
      , toData initGenesisHash
      , toData initUtxo
      , toData initCommittee
      , toData initMint
      ]

instance Show InitSidechainParams where
  show = genericShow
