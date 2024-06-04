module TrustlessSidechain.Utils.Codecs
  ( byteArrayCodec
  , transactionInputCodec
  , thresholdCodec
  , atmsKindCodec
  , scParamsCodec
  , pubKeyHashCodec
  , encodeInitTokenStatusData
  ) where

import Contract.Prelude

import Aeson as Aeson
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.CborBytes (CborBytes(CborBytes))
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToHex
  , hexToByteArray
  )
import Contract.Transaction
  ( TransactionInput(TransactionInput)
  )
import Data.Argonaut (Json)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Map (Map)
import Data.Map as Map
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Foreign.Object as Object
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
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
import TrustlessSidechain.Governance
  ( GovernanceAuthority
  , mkGovernanceAuthority
  )
import TrustlessSidechain.Options.Parsers as Parsers
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))

-- Note [BigInt values and JSON]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- `BigInt` values are not supported in JSON and coercing them to
-- `Number` can lead to loss of information. `Argonaut.Json` does not
-- support `BigInt` encoding or decoding. Therefore conversions of
-- `BigInt` values to JSON must compensate, for example by being partial
-- or by first converting values to strings. See the `BigInt` documentation at
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/

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
        transactionId ← (decodeCbor <<< wrap) =<< hexToByteArray txId
        in
          TransactionInput
            { transactionId
            , index
            }
      _ → Nothing

  fromF ∷ TransactionInput → String
  fromF (TransactionInput txIn) = txHashStr <> "#" <> indexStr
    where
    indexStr = UInt.toString txIn.index
    txHashStr = byteArrayToHex $ unwrap $ encodeCbor $ txIn.transactionId

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

-- | JSON codec for PubKeyHash.
governanceAuthorityCodec ∷ CA.JsonCodec GovernanceAuthority
governanceAuthorityCodec = CA.prismaticCodec "GovernanceAuthority"
  (Just <<< mkGovernanceAuthority)
  unwrap
  pubKeyHashCodec

-- | See Note [BigInt values and JSON]
scParamsCodec ∷ CA.JsonCodec SidechainParams
scParamsCodec =
  wrapIso SidechainParams $
    ( CAR.object "sidechainParameters"
        { chainId: bigIntCodec
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
        , governanceAuthority: governanceAuthorityCodec
        }
    )

pubKeyHashCodec ∷ CA.JsonCodec PaymentPubKeyHash
pubKeyHashCodec = CA.prismaticCodec "PaymentPubKeyHash"
  (Just <<< wrap)
  unwrap
  ed25519KeyHashCodec

-- | JSON codec for ed25519KeyHash.
ed25519KeyHashCodec ∷ CA.JsonCodec Ed25519KeyHash
ed25519KeyHashCodec = CA.prismaticCodec "Ed25519KeyHash"
  (CborBytes >>> decodeCbor)
  (unwrap <<< encodeCbor)
  byteArrayCodec

-- | Json encoder for InitTokenStatusResp
-- | See Note [BigInt values and JSON]
-- | This function chooses to use unsafeToInt before converting to Number
-- | since the BigInt values in all reasonable cases should be within the
-- | range of Int.
-- | AssetName is encoded as the defined in the EncodeAeson instance
-- | provided in cardano-transaction-lib.
encodeInitTokenStatusData ∷ Map AssetName BigNum → Json
encodeInitTokenStatusData = J.fromObject <<< Object.fromFoldable <<< toKvs
  where
  toKvs m = Array.zipWith
    ( \k v → (Aeson.stringifyAeson $ Aeson.encodeAeson k) /\
        CA.encode bigNumCodec v
    )
    (Array.fromFoldable $ Map.keys m)
    (Array.fromFoldable $ Map.values m)

-- | JSON codec for `BigInt`.
-- | See Note [BigInt values and JSON]
bigIntCodec ∷ CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt"
  (Just <<< BigInt.fromInt)
  unsafeToInt
  CA.int

-- | JSON codec for `BigInt`.
-- | See Note [BigInt values and JSON]
bigNumCodec ∷ CA.JsonCodec BigNum
bigNumCodec = CA.prismaticCodec "BigInt"
  (Just <<< BigNum.fromInt)
  (BigNum.toInt >>> unsafePartial fromJust)
  CA.int

unsafeToInt ∷ BigInt → Int
unsafeToInt x = unsafePartial $ fromJust $ BigInt.toInt x
