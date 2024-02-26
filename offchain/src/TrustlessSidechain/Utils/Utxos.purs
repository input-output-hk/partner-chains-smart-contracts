-- | `Utils.Utxos` provides some utility functions for querying utxos.
module TrustlessSidechain.Utils.Utxos
  ( findUtxoByValueAt
  , getOwnUTxOsTotalValue
  , getOwnUTxOs
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Monad (Contract)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Utxos (UtxoMap)
import Contract.Utxos as Utxos
import Contract.Value (Value)
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Map as Map
import TrustlessSidechain.Utils.Address
  ( getOwnWalletAddress
  )

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

getOwnUTxOsTotalValue ∷ Contract Value
getOwnUTxOsTotalValue = do
  ownUtxos ← getOwnUTxOs
  pure
    $ foldMap
        ( \( TransactionOutputWithRefScript
               { output: TransactionOutput { amount } }
           ) → amount
        )
    $ Map.values ownUtxos

getOwnUTxOs ∷ Contract UtxoMap
getOwnUTxOs = do
  ownAddr ← getOwnWalletAddress
  Utxos.utxosAt ownAddr
