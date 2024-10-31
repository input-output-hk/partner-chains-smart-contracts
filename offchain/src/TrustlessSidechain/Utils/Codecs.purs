module TrustlessSidechain.Utils.Codecs
  ( byteArrayCodec
  , transactionInputCodec
  , scParamsCodec
  , pubKeyHashCodec
  ) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
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
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.Governance.Admin
  ( GovernanceAuthority
  , mkGovernanceAuthority
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))

-- | JSON codec converting between a bytestring and its hexadecimal representation
byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray"
  hexToByteArray
  byteArrayToHex
  CA.string

-- | JSON codec converiting between a Plutus transaction input and the conventional
-- | CLI format (TX_ID#TX_IDX)
transactionInputCodec :: CA.JsonCodec TransactionInput
transactionInputCodec =
  CA.prismaticCodec "TransactionInput" toF fromF CA.string
  where
  toF :: String -> Maybe TransactionInput
  toF txIn =
    case split (Pattern "#") txIn of
      [ txId, txIdx ] -> ado
        index <- UInt.fromString txIdx
        transactionId <- (decodeCbor <<< wrap) =<< hexToByteArray txId
        in
          TransactionInput
            { transactionId
            , index
            }
      _ -> Nothing

  fromF :: TransactionInput -> String
  fromF (TransactionInput txIn) = txHashStr <> "#" <> indexStr
    where
    indexStr = UInt.toString txIn.index
    txHashStr = byteArrayToHex $ unwrap $ encodeCbor $ txIn.transactionId

-- | JSON codec for PubKeyHash.
governanceAuthorityCodec :: CA.JsonCodec GovernanceAuthority
governanceAuthorityCodec = CA.prismaticCodec "GovernanceAuthority"
  (Just <<< mkGovernanceAuthority)
  unwrap
  pubKeyHashCodec

scParamsCodec :: CA.JsonCodec SidechainParams
scParamsCodec =
  wrapIso SidechainParams $
    ( CAR.object "sidechainParameters"
        { genesisUtxo: transactionInputCodec
        , governanceAuthority: governanceAuthorityCodec
        }
    )

pubKeyHashCodec :: CA.JsonCodec PaymentPubKeyHash
pubKeyHashCodec = CA.prismaticCodec "PaymentPubKeyHash"
  (Just <<< wrap)
  unwrap
  ed25519KeyHashCodec

-- | JSON codec for ed25519KeyHash.
ed25519KeyHashCodec :: CA.JsonCodec Ed25519KeyHash
ed25519KeyHashCodec = CA.prismaticCodec "Ed25519KeyHash"
  (CborBytes >>> decodeCbor)
  (unwrap <<< encodeCbor)
  byteArrayCodec
