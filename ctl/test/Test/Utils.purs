module Test.Utils (toTxIn, getUniqueUtxoAt) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.Utxos as Utxos
import Data.Map as Map
import Data.UInt as UInt

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
