-- | `Utils.Utxos` provides some utility functions for querying utxos.
module TrustlessSidechain.Utils.Utxos
  ( findUtxoByValueAt
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos as Utxos
import Contract.Value (Value)
import Data.FoldableWithIndex as FoldableWithIndex

-- | `findUtxoAtByValue addr p` finds all utxos at the validator address `addr`
-- | using `Contract.Utxos.utxosAt`, then looks for the first utxo which satisfies
-- | `p` (if such utxo exists).
-- |
-- | Note: this does a linear scan over all utxos at the given address `addr`
findUtxoByValueAt ∷
  Address →
  (Value → Boolean) →
  Contract
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findUtxoByValueAt addr p = do
  scriptUtxos ← Utxos.utxosAt addr
  let
    go _txIn txOut = p (unwrap (unwrap txOut).output).amount
  pure $ FoldableWithIndex.findWithIndex go scriptUtxos
