module TrustlessSidechain.Utils.Codecs
  ( byteArrayCodec
  , transactionInputCodec
  , thresholdCodec
  , atmsKindCodec
  , scParamsCodec
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash(TransactionHash))
import Ctl.Internal.Types.Transaction (TransactionInput(TransactionInput))
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds
      ( ATMSPlainEcdsaSecp256k1
      , ATMSPlainSchnorrSecp256k1
      , ATMSDummy
      , ATMSPoK
      , ATMSMultisignature
      )
  )
import TrustlessSidechain.Options.Parsers as Parsers
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))

-- | JSON codec converting between a bytestring and its hexadecimal representation
byteArrayCodec ∷ CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray"
  hexToByteArray
  byteArrayToHex
  CA.string

-- | JSON codec converiting between a Plutus transaction input and the conventional
-- | CLI format (TX_ID#TX_IDX)
transactionInputCodec ∷ CA.JsonCodec TransactionInput
transactionInputCodec =
  CA.prismaticCodec "TransactionInput" toF fromF CA.string
  where
  toF ∷ String → Maybe TransactionInput
  toF txIn =
    case split (Pattern "#") txIn of
      [ txId, txIdx ] → ado
        index ← UInt.fromString txIdx
        in
          TransactionInput
            { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
            , index
            }
      _ → Nothing

  fromF ∷ TransactionInput → String
  fromF (TransactionInput txIn) = txHashStr <> "#" <> indexStr
    where
    indexStr = UInt.toString txIn.index
    txHashStr = case txIn.transactionId of
      TransactionHash txId → byteArrayToHex txId

-- | JSON codec for the atms kind. Note that this should match
-- | `TrustlessSidechain.Options.Parsers.parseATMSKind` (both the parsing and the
-- | printing for documentation)
atmsKindCodec ∷ CA.JsonCodec ATMSKinds
atmsKindCodec =
  CA.prismaticCodec "atms-kind" toF fromF CA.string
  where
  toF ∷ String → Maybe ATMSKinds
  toF str = case Parsers.parseATMSKind str of
    Right atmsKind → Just atmsKind
    Left _err → Nothing

  fromF ∷ ATMSKinds → String
  fromF ATMSPlainEcdsaSecp256k1 =
    "plain-ecdsa-secp256k1"
  fromF ATMSPlainSchnorrSecp256k1 =
    "plain-schnorr-secp256k1"
  fromF ATMSDummy = "dummy"
  fromF ATMSPoK = "pok"
  fromF ATMSMultisignature = "multisignature"

-- | `thresholdCodec` is the codec for the threshold in `Options.Types.Config`.
-- | Note that this codec has no relation to the `thresholdNumerator` and
-- | `thresholdDenominator` fields in `SidechainParams`.
thresholdCodec ∷ CA.JsonCodec { numerator ∷ Int, denominator ∷ Int }
thresholdCodec = CA.object "threshold" $
  CAR.record
    { numerator: CA.int
    , denominator: CA.int
    }

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
