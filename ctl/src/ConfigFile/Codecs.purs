module ConfigFile.Codecs (configCodec) where

import Contract.Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (ServerConfig)
import Contract.Prim.ByteArray
  ( byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash(..))
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Options.Types (Config)
import Types.ByteArray (ByteArray)
import Types.Transaction (TransactionInput(TransactionInput))

configCodec ∷ CA.JsonCodec Config
configCodec =
  CA.object "Config file"
    ( CAR.record
        { sidechainParameters: CAC.maybe scParamsCodec
        , paymentSigningKeyFile: CAC.maybe CA.string
        , stakeSigningKeyFile: CAC.maybe CA.string
        , runtimeConfig: CAC.maybe runtimeConfigCodec
        }
    )
  where
  scParamsCodec =
    ( CAR.object "sidechainParameters"
        { chainId: CAC.maybe CA.int
        , genesisHash: CAC.maybe byteArrayCodec
        , genesisMint: CAC.maybe transactionInputCodec
        , genesisUtxo: CAC.maybe transactionInputCodec
        }
    )
  runtimeConfigCodec =
    ( CAR.object "runtimeConfig"
        { ogmios: CAC.maybe serverConfigCodec
        , ogmiosDatumCache: CAC.maybe serverConfigCodec
        , ctlServer: CAC.maybe serverConfigCodec
        , network: CAC.maybe networkIdCodec
        }
    )

serverConfigCodec ∷ CA.JsonCodec ServerConfig
serverConfigCodec = CAR.object "serverConfig"
  { host: CA.string
  , port: CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int
  , secure: CA.boolean
  , path: CAC.maybe CA.string
  }

networkIdCodec ∷ CA.JsonCodec NetworkId
networkIdCodec = CA.prismaticCodec "Network" dec enc CA.string
  where
  dec = case _ of
    "mainnet" → Just MainnetId
    "testnet" → Just TestnetId
    _ → Nothing
  enc = case _ of
    MainnetId → "mainnet"
    TestnetId → "testnet"

byteArrayCodec ∷ CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex
  CA.string

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
  fromF (TransactionInput txIn) = txHashStr <> "#" <> indexStr
    where
    indexStr = UInt.toString txIn.index
    txHashStr = case txIn.transactionId of
      TransactionHash x → byteArrayToHex x
