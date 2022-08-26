module Config (readOptions, encodeOptions, optExample, readConfigFile) where

import Contract.Prelude
import Test.Unit.Console

import CommitteCandidateValidator (PubKey, Signature)
import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash(..))
import Data.Argonaut.Core as J
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
import Node.Buffer.Class as Buff
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readFile)
import Node.Path (FilePath)
import Options.Types (Endpoint(..), Options)
import SidechainParams (SidechainParams(..))
import Type.Proxy (Proxy(..))
import Types.Transaction (TransactionInput(TransactionInput))

encodeOptions :: Options -> String
encodeOptions = CA.encode optionsCodec >>> J.stringify

optExample :: Options
optExample =
  { scParams: SidechainParams
      { chainId: BInt.fromInt 1
      , genesisHash: hexToByteArrayUnsafe "genesisHash"
      , genesisMint: Nothing
      , genesisUtxo: TransactionInput
          { transactionId: TransactionHash (hexToByteArrayUnsafe "TxHash")
          , index: UInt.fromInt 2
          }
      }
  , skey: "skey"
  , endpoint: MintAct { amount: 2 }
  }

readOptions :: String -> Effect (Either CA.JsonDecodeError Options)
readOptions str = do
  print str
  pure $ CA.decode optionsCodec $ J.fromString str

readConfigFile :: FilePath -> Effect String
readConfigFile path = readFile path >>= Buff.toString ASCII

optionsCodec :: CA.JsonCodec Options
optionsCodec = do
  CA.object "Options"
    ( CAR.record
        { scParams: scParamsCodec
        , skey: CA.string
        , endpoint: endpointCodec
        }
    )

scParamsCodec :: CA.JsonCodec SidechainParams
scParamsCodec = wrapIso SidechainParams
  ( CAR.object "SidechainParams"
      { chainId: bigIntCodec
      , genesisHash: byteArrayCodec
      , genesisMint: CAC.maybe transactionInputCodec
      , genesisUtxo: transactionInputCodec
      }
  )

endpointCodec :: CA.JsonCodec Endpoint
endpointCodec = dimap toVariant fromVariant $ CAV.variantMatch
  { mintAct: Right mintCodec
  , burnAct: Right burnCodec
  , committeeCandidateReg: Right committeeCandidateRegCodec
  , committeeCandidateDereg: Right committeeCandidateDeregCodec
  }
  where
  -- necessary boilerplate
  toVariant = case _ of
    MintAct x → V.inj (Proxy ∷ _ "mintAct") x
    BurnAct x → V.inj (Proxy ∷ _ "burnAct") x
    CommitteeCandidateReg x → V.inj (Proxy ∷ _ "committeeCandidateReg") x
    CommitteeCandidateDereg x → V.inj (Proxy ∷ _ "committeeCandidateDereg") x

  -- necessary boilerplate
  fromVariant = V.match
    { mintAct: MintAct
    , burnAct: BurnAct
    , committeeCandidateReg: CommitteeCandidateReg
    , committeeCandidateDereg: CommitteeCandidateDereg
    }

  mintCodec :: CA.JsonCodec { amount :: Int }
  mintCodec = CAR.object "mint" { amount: CA.int }

  burnCodec :: CA.JsonCodec { amount :: Int, recipient :: String }
  burnCodec = CAR.object "burn" { amount: CA.int, recipient: CA.string }

  committeeCandidateRegCodec ::
    CA.JsonCodec
      { spoPubKey :: PubKey
      , sidechainPubKey :: PubKey
      , spoSig :: Signature
      , sidechainSig :: Signature
      , inputUtxo :: TransactionInput
      }
  committeeCandidateRegCodec = CAR.object "committeeCandidateReg"
    { spoPubKey: pubKeyCodec
    , sidechainPubKey: pubKeyCodec
    , spoSig: signatureCodec
    , sidechainSig: signatureCodec
    , inputUtxo: transactionInputCodec
    }

  committeeCandidateDeregCodec :: CA.JsonCodec { spoPubKey :: PubKey }
  committeeCandidateDeregCodec = CAR.object "committeeCandidateDereg"
    { spoPubKey: pubKeyCodec }

-- Necessary Codecs

pubKeyCodec :: CA.JsonCodec PubKey
pubKeyCodec = byteArrayCodec

signatureCodec :: CA.JsonCodec Signature
signatureCodec = byteArrayCodec

byteArrayCodec :: CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex
  CA.string

bigIntCodec :: CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt" BInt.fromString BInt.toString CA.string

transactionInputCodec :: CA.JsonCodec TransactionInput
transactionInputCodec = CA.prismaticCodec "TransactionInput" toF fromF CA.string
  where
  toF txIn =
    case split (Pattern "#") txIn of
      [ txId, txIdx ] -> ado
        index <- UInt.fromString txIdx
        in
          TransactionInput
            { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
            , index
            }
      _ -> Nothing

  fromF :: TransactionInput -> String
  fromF (TransactionInput txIn) = indexStr <> "#" <> txHashStr
    where
    indexStr = show txIn.index
    txHashStr = case txIn.transactionId of
      TransactionHash x -> byteArrayToHex x
