module Utils.Codecs
  ( byteArrayCodec
  , transactionInputCodec
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray
  ( ByteArray
  , byteArrayToHex
  , hexToByteArray
  , hexToByteArrayUnsafe
  )
import Contract.Transaction (TransactionHash(TransactionHash))
import Data.Codec.Argonaut as CA
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Types.Transaction (TransactionInput(TransactionInput))

-- | JSON codec converting between a bytestring and its hexadecimal representation
byteArrayCodec ∷ CA.JsonCodec ByteArray
byteArrayCodec = CA.prismaticCodec "ByteArray"
  hexToByteArray
  byteArrayToHex
  CA.string

-- | JSON codec converiting between a Plutus transaction input and the conventional
-- CLI format (TX_ID#TX_IDX)
transactionInputCodec ∷ CA.JsonCodec TransactionInput
transactionInputCodec =
  CA.prismaticCodec "TransactionInput" toF fromF CA.string
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
      TransactionHash txId → byteArrayToHex txId
