module ConfigFile.Codecs
  ( configCodec
  , scParamsCodec
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray
  ( byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash(..))
import Data.BigInt (BigInt)
import Data.BigInt as BInt
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Options.Types (Config)
import SidechainParams (SidechainParams(..))
import Types.ByteArray (ByteArray)
import Types.Transaction (TransactionInput(TransactionInput))

configCodec ∷ CA.JsonCodec Config
configCodec =
  CA.object "Config file"
    ( CAR.record
        { sidechainParameters: CAC.maybe scParamsCodec
        , signingKeyFile: CAC.maybe CA.string
        }
    )

scParamsCodec ∷ CA.JsonCodec SidechainParams
scParamsCodec = wrapIso SidechainParams
  ( CAR.object "sidechainParameters"
      { chainId: bigIntCodec
      , genesisHash: byteArrayCodec
      , genesisMint: CAC.maybe transactionInputCodec
      , genesisUtxo: transactionInputCodec
      }
  )

byteArrayCodec ∷ CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray" hexToByteArray byteArrayToHex
  CA.string

bigIntCodec ∷ CA.JsonCodec BigInt
bigIntCodec = CA.prismaticCodec "BigInt" BInt.fromNumber BInt.toNumber CA.number

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
