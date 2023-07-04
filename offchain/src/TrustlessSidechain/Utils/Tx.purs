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
import Data.Bifunctor (lmap)
import TrustlessSidechain.Utils.Logging as Utils.Logging

-- | `submitAndAwaitTx` takes lookups and constraints, uses them to build a
-- | transaction, and submits it.
submitAndAwaitTx ∷
  Utils.Logging.Location →
  { lookups ∷ ScriptLookups Void
  , constraints ∷ TxConstraints Void Void
  } →
  Contract TransactionHash
submitAndAwaitTx loc { lookups, constraints } = do
  -- Logging
  ----------------------------------------
  let
    msg = Utils.Logging.mkReport loc

  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ msg $ "Submitted transaction: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg "Transaction submitted successfully."
  pure txId
