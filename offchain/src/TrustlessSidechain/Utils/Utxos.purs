-- | `Utils.Utxos` provides some utility functions for querying utxos.
module TrustlessSidechain.Utils.Utxos
  ( findUtxoByValueAt
  , getOwnUTxOsTotalValue
  , getOwnUTxOs
  , plutusScriptFromTxIn
  ) where

import Contract.Prelude

import Cardano.Types.Address (Address)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value (Value)
import Cardano.Types.Value (sum) as Value
import Contract.Utxos (UtxoMap)
import Data.Array as Array
import Data.FoldableWithIndex as FoldableWithIndex
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except (note) as Run
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logWarn')
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
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
    (Maybe { index ∷ TransactionInput, value ∷ TransactionOutput })
findUtxoByValueAt addr p = do
  scriptUtxos ← Effect.utxosAt addr
  let
    go _txIn txOut = p (unwrap txOut).amount
  pure $ FoldableWithIndex.findWithIndex go scriptUtxos

getOwnUTxOsTotalValue ∷
  ∀ r. Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) Value
getOwnUTxOsTotalValue = do
  ownUtxos ← getOwnUTxOs
  Run.note (GenericInternalError "Couldn't add up own utxo values.")
    $ Value.sum
    $ Array.fromFoldable
    $ map
        ( \( TransactionOutput { amount }
           ) → amount
        )
    $ Map.values ownUtxos

getOwnUTxOs ∷
  ∀ r. Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) UtxoMap
getOwnUTxOs = do
  ownAddr ← getOwnWalletAddress
  Effect.utxosAt ownAddr

-- | Retrieve plutusScript from TransactionInput
plutusScriptFromTxIn ∷
  ∀ r.
  TransactionInput →
  Run (APP r) (Maybe PlutusScript)
plutusScriptFromTxIn txIn = do
  m ← Effect.getUtxo txIn
  case m of
    Just (TransactionOutput { scriptRef: Just (PlutusScriptRef plutusScript) }) →
      pure $ Just plutusScript
    _ →
      logWarn' "Failed to retrieve PlutusScript from TransactionInput" *>
        pure Nothing
