-- | 'Utils.Utxos' provides some utility functions for querying utxos.
module Utils.Utxos where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos as Utxos
import Contract.Value (Value)
import Data.FoldableWithIndex as FoldableWithIndex

-- | @'findUtxoAtByValue' addr p@ finds all utxos at the validator address 'addr'
-- using 'Contract.Utxos.utxosAt', then looks for the first utxo which satisfies
-- @p@ (if such utxos exist).
findUtxoByValueAt ∷
  Address →
  (Value → Boolean) →
  Contract ()
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findUtxoByValueAt addr p = do
  scriptUtxos ←
    Monad.liftedM
      "error 'Utils.Utxos.findUtxoByValueAt': 'Contract.Utxos.utxosAt' failed"
      $ Utxos.utxosAt addr
  let
    go _txIn txOut = p (unwrap (unwrap txOut).output).amount
  pure $ FoldableWithIndex.findWithIndex go scriptUtxos
