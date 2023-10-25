module TrustlessSidechain.Utils.Tx
  ( submitAndAwaitTx
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)

-- | `submitAndAwaitTx` takes lookups and constraints, uses them to build a
-- | transaction, and submits it.
submitAndAwaitTx ∷
  { lookups ∷ ScriptLookups Void
  , constraints ∷ TxConstraints Void Void
  } →
  Contract TransactionHash
submitAndAwaitTx { lookups, constraints } = do
  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ "Submitted transaction: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ "Transaction submitted successfully."
  pure txId
