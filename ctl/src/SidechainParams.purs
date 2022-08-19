module SidechainParams where

import Contract.Prelude

import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Data.BigInt (BigInt)
import Types.Transaction (TransactionInput)

newtype SidechainParams = SidechainParams
  { chainId ∷ BigInt
  , genesisHash ∷ String
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
