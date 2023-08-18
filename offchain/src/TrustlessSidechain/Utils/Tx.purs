module TrustlessSidechain.Utils.Tx
  ( submitAndAwaitTx
  ) where

import Contract.Prelude

import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Data.Set (Set)

-- | `submitAndAwaitTx` takes lookups and constraints, uses them to build a
-- | transaction, and submits it.
submitAndAwaitTx ∷
  Set TransactionInput →
  { lookups ∷ ScriptLookups Void
  , constraints ∷ TxConstraints Void Void
  } →
  Contract TransactionHash
submitAndAwaitTx forbiddenUtxos { lookups, constraints } = do
  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  let
    balanceTxConstraints = BalanceTxConstraints.mustNotSpendUtxosWithOutRefs
      forbiddenUtxos
  bsTx ← liftedE (balanceTxWithConstraints ubTx balanceTxConstraints)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ "Submitted transaction: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Transaction submitted successfully."
  pure txId
