-- | `Utils.Utxos` provides some utility functions for querying utxos.
module TrustlessSidechain.Utils.Utxos
  ( findUtxoByValueAt
  , getOwnUTxOsTotalValue
  , getOwnUTxOs
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Utxos (UtxoMap)
import Contract.Value (Value)
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Address
  ( getOwnWalletAddress
  )
import Type.Row (type (+))

-- | `findUtxoAtByValue addr p` finds all utxos at the validator address `addr`
-- | using `Contract.Utxos.utxosAt`, then looks for the first utxo which satisfies
-- | `p` (if such utxo exists).
-- |
-- | Note: this does a linear scan over all utxos at the given address `addr`
findUtxoByValueAt ∷
  ∀ r.
  Address →
  (Value → Boolean) →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutputWithRefScript })
findUtxoByValueAt addr p = do
  scriptUtxos ← Effect.utxosAt addr
  let
    go _txIn txOut = p (unwrap (unwrap txOut).output).amount
  pure $ FoldableWithIndex.findWithIndex go scriptUtxos

getOwnUTxOsTotalValue ∷
  ∀ r. Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) Value
getOwnUTxOsTotalValue = do
  ownUtxos ← getOwnUTxOs
  pure
    $ foldMap
        ( \( TransactionOutputWithRefScript
               { output: TransactionOutput { amount } }
           ) → amount
        )
    $ Map.values ownUtxos

getOwnUTxOs ∷ ∀ r. Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) UtxoMap
getOwnUTxOs = do
  ownAddr ← getOwnWalletAddress
  Effect.utxosAt ownAddr
