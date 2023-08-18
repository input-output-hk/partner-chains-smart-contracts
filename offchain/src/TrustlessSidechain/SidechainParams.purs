module TrustlessSidechain.SidechainParams where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import TrustlessSidechain.Governance as Governance

newtype SidechainParams = SidechainParams
  { chainId ∷ BigInt
  , genesisHash ∷ ByteArray
  , genesisUtxo ∷ TransactionInput
  ,
    -- `thresholdNumerator` is the numerator of the ratio required for the
    -- committee to verify that committee has signed something (e.g. when
    -- updating the committee hash, or saving a new merkle root).
    thresholdNumerator ∷ BigInt
  ,
    -- `thresholdDenominator` is the denominator of the ratio required for the
    -- committee to verify that committee has signed something (e.g. when
    -- updating the committee hash, or saving a new merkle root).
    thresholdDenominator ∷ BigInt
  , -- Governance mechanism.  We temporarily rely on using a single master key
    -- that can authorize any action requiring permission from the governing
    -- committee.
    governanceAuthority ∷ Governance.GovernanceAuthority
  }

derive instance Generic SidechainParams _

derive instance Newtype SidechainParams _

instance ToData SidechainParams where
  toData
    ( SidechainParams
        { chainId
        , genesisHash
        , genesisUtxo
        , thresholdNumerator
        , thresholdDenominator
        , governanceAuthority
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData chainId
      , toData genesisHash
      , toData genesisUtxo
      , toData thresholdNumerator
      , toData thresholdDenominator
      , toData governanceAuthority
      ]

instance Show SidechainParams where
  show = genericShow
