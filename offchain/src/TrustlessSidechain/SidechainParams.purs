module TrustlessSidechain.SidechainParams where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Control.Alternative (guard)
import Data.BigInt (BigInt)

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
  }

derive instance Generic SidechainParams _

derive instance Newtype SidechainParams _

derive newtype instance Eq SidechainParams

instance ToData SidechainParams where
  toData
    ( SidechainParams
        { chainId
        , genesisHash
        , genesisUtxo
        , thresholdNumerator
        , thresholdDenominator
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData chainId
      , toData genesisHash
      , toData genesisUtxo
      , toData thresholdNumerator
      , toData thresholdDenominator
      ]

instance FromData SidechainParams where
  fromData = case _ of
    Constr ix [ cid, gh, gu, tn, td ] → do
      guard (ix == BigNum.fromInt 0)
      chainId ← fromData cid
      genesisHash ← fromData gh
      genesisUtxo ← fromData gu
      thresholdNumerator ← fromData tn
      thresholdDenominator ← fromData td
      pure $ SidechainParams
        { chainId
        , genesisHash
        , genesisUtxo
        , thresholdNumerator
        , thresholdDenominator
        }
    _ → Nothing

instance Show SidechainParams where
  show = genericShow
