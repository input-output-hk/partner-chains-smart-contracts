module ConfigFile.Codecs
  ( optionsCodec
  , scParamsCodec
  ) where

import Contract.Prelude

import CommitteCandidateValidator (PubKey, Signature)
import Contract.Prim.ByteArray
  ( byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash(..))
import Data.BigInt (BigInt)
import Data.BigInt as BInt
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Profunctor (dimap, wrapIso)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Data.Variant as V
import Options.Types (Endpoint(..), Options)
import SidechainParams (SidechainParams(..))
import Type.Proxy (Proxy(..))
import Types.ByteArray (ByteArray)
import Types.Transaction (TransactionInput(TransactionInput))

optionsCodec ∷ CA.JsonCodec (Options SidechainParams)
optionsCodec =
  CA.object "Options"
    ( CAR.record
        { scParams: scParamsCodec
        , skey: CA.string
        , endpoint: endpointCodec
        }
    )

scParamsCodec ∷ CA.JsonCodec SidechainParams
scParamsCodec = wrapIso SidechainParams
  ( CAR.object "SidechainParams"
      { chainId: bigIntCodec
      , genesisHash: byteArrayCodec
      , genesisMint: CAC.maybe transactionInputCodec
      , genesisUtxo: transactionInputCodec
      }
  )

endpointCodec ∷ CA.JsonCodec Endpoint
endpointCodec = dimap toVariant fromVariant $ CAV.variantMatch
  { mintAct: Right mintCodec
  , burnAct: Right burnCodec
  , committeeCandidateReg: Right committeeCandidateRegCodec
  , committeeCandidateDereg: Right committeeCandidateDeregCodec
  , getAddrs: Left unit
  }
  where
  -- necessary boilerplate
  toVariant = case _ of
    MintAct x → V.inj (Proxy ∷ _ "mintAct") x
    BurnAct x → V.inj (Proxy ∷ _ "burnAct") x
    CommitteeCandidateReg x → V.inj (Proxy ∷ _ "committeeCandidateReg") x
    CommitteeCandidateDereg x → V.inj (Proxy ∷ _ "committeeCandidateDereg") x
    GetAddrs → V.inj (Proxy ∷ _ "getAddrs") unit

  -- necessary boilerplate
  fromVariant = V.match
    { mintAct: MintAct
    , burnAct: BurnAct
    , committeeCandidateReg: CommitteeCandidateReg
    , committeeCandidateDereg: CommitteeCandidateDereg
    , getAddrs: \_ → GetAddrs
    }

  mintCodec ∷ CA.JsonCodec { amount ∷ Int }
  mintCodec = CAR.object "mint" { amount: CA.int }

  burnCodec ∷ CA.JsonCodec { amount ∷ Int, recipient ∷ ByteArray }
  burnCodec = CAR.object "burn" { amount: CA.int, recipient: byteArrayCodec }

  committeeCandidateRegCodec ∷
    CA.JsonCodec
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ PubKey
      , spoSig ∷ Signature
      , sidechainSig ∷ Signature
      , inputUtxo ∷ TransactionInput
      }
  committeeCandidateRegCodec = CAR.object "committeeCandidateReg"
    { spoPubKey: pubKeyCodec
    , sidechainPubKey: pubKeyCodec
    , spoSig: signatureCodec
    , sidechainSig: signatureCodec
    , inputUtxo: transactionInputCodec
    }

  committeeCandidateDeregCodec ∷ CA.JsonCodec { spoPubKey ∷ PubKey }
  committeeCandidateDeregCodec = CAR.object "committeeCandidateDereg"
    { spoPubKey: pubKeyCodec }

pubKeyCodec ∷ CA.JsonCodec PubKey
pubKeyCodec = byteArrayCodec

signatureCodec ∷ CA.JsonCodec Signature
signatureCodec = byteArrayCodec

byteArrayCodec ∷ CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex
  CA.string

bigIntCodec ∷ CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt" BInt.fromString BInt.toString CA.string

transactionInputCodec ∷ CA.JsonCodec TransactionInput
transactionInputCodec = CA.prismaticCodec "TransactionInput" toF fromF CA.string
  where
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
  fromF (TransactionInput txIn) = indexStr <> "#" <> txHashStr
    where
    indexStr = show txIn.index
    txHashStr = case txIn.transactionId of
      TransactionHash x → byteArrayToHex x
