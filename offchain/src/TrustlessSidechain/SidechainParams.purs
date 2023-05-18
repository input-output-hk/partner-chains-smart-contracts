module TrustlessSidechain.SidechainParams where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Utils.Codecs (byteArrayCodec, transactionInputCodec)

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

instance Show SidechainParams where
  show = genericShow

scParamsCodec ∷ CA.JsonCodec SidechainParams
scParamsCodec =
  wrapIso SidechainParams $
    ( CAR.object "sidechainParameters"
        { chainId: chainIdCodec
        , genesisHash: byteArrayCodec
        , genesisUtxo: transactionInputCodec
        , thresholdNumerator:
            CA.prismaticCodec "thresholdNumerator"
              (Just <<< BigInt.fromInt)
              unsafeToInt
              CA.int
        , thresholdDenominator:
            CA.prismaticCodec "thresholdDenominator"
              (Just <<< BigInt.fromInt)
              unsafeToInt
              CA.int
        }
    )
  where
  chainIdCodec ∷ CA.JsonCodec BigInt
  chainIdCodec = CA.prismaticCodec "chainId"
    (Just <<< BigInt.fromInt)
    unsafeToInt
    CA.int

  unsafeToInt ∷ BigInt → Int
  unsafeToInt x = unsafePartial $ fromJust $ BigInt.toInt x
