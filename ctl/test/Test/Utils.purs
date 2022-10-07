module Test.Utils (toTxIn, getUniqueUtxoAt, paymentPubKeyHashToByteArray) where

import Contract.Prelude

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionHash(..)
  , TransactionInput(..)
  , TransactionOutputWithRefScript
  )
import Contract.Utxos as Utxos
import Data.Map as Map
import Data.UInt as UInt
import Serialization.Hash as Hash

toTxIn ∷ String → Int → TransactionInput
toTxIn txId txIdx =
  TransactionInput
    { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
    , index: UInt.fromInt txIdx
    }

-- |  @'getUniqueUtxoAt' addr@ gets the first utxo at the given address, and throws an
-- error if there is NOT exactly one utxo at this address.
getUniqueUtxoAt ∷
  Address → Contract () (Tuple TransactionInput TransactionOutputWithRefScript)
getUniqueUtxoAt addr = do
  utxoMap ← Monad.liftedM "Failed to get utxos at script address" $
    Utxos.utxosAt addr
  let
    err = Monad.throwContractError
      $ "Expected exactly one script address but got:"
      <> show utxoMap

  case Map.findMin utxoMap of
    Just { key, value }
      | length utxoMap == 1 → pure $ key /\ value
      | otherwise → err
    Nothing → err

-- | Coerces a 'PaymentPubKeyHash' to a 'ByteArray'. This is useful when making
-- the recipient for the 'MerkleTreeEntry'.
paymentPubKeyHashToByteArray ∷ PaymentPubKeyHash → ByteArray
paymentPubKeyHashToByteArray =
  unwrap <<< Hash.ed25519KeyHashToBytes <<< unwrap <<< unwrap
