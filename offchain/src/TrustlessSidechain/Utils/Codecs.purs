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
import Contract.Address (PubKeyHash)
import Cardano.Plutus.Types.Map as Plutus.Map
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Contract.Value (TokenName)
import Cardano.Serialization.Lib
  ( ed25519KeyHashFromBytes
  , ed25519KeyHashToBytes
  )
import Data.Argonaut (Json)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Foreign.Object as Object
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

-- | JSON codec for PubKeyHash.
pubKeyHashCodec ∷ CA.JsonCodec PubKeyHash
pubKeyHashCodec = CA.prismaticCodec "PubKeyHash"
  (ed25519KeyHashFromBytes >=> wrap >>> pure)
  (unwrap <<< ed25519KeyHashToBytes <<< unwrap)
  byteArrayCodec

-- | Json encoder for InitTokenStatusResp
-- | See Note [BigInt values and JSON]
-- | This function chooses to use unsafeToInt before converting to Number
-- | since the BigInt values in all reasonable cases should be within the
-- | range of Int.
-- | TokenName is encoded as the defined in the EncodeAeson instance
-- | provided in cardano-transaction-lib.
encodeInitTokenStatusData ∷ Plutus.Map.Map TokenName BigInt → Json
encodeInitTokenStatusData = J.fromObject <<< Object.fromFoldable <<< toKvs
  where
  toKvs m = Array.zipWith
    ( \k v → (Aeson.stringifyAeson $ Aeson.encodeAeson k) /\
        CA.encode bigIntCodec v
    )
    (Plutus.Map.keys m)
    (Plutus.Map.values m)

-- | JSON codec for `BigInt`.
-- | See Note [BigInt values and JSON]
bigIntCodec ∷ CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt"
  (Just <<< BigInt.fromInt)
  unsafeToInt
  CA.int

unsafeToInt ∷ BigInt → Int
unsafeToInt x = unsafePartial $ fromJust $ BigInt.toInt x
