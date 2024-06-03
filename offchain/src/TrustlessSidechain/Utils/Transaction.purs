module TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  , balanceSignAndSubmitWithoutSpendingUtxo
  , txHashToByteArray
  ) where

import Contract.Prelude

import Data.ByteArray (ByteArray)
import Cardano.AsCbor (encodeCbor)
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction (TransactionHash, TransactionInput)
import Contract.TxConstraints (TxConstraints)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction
  ( awaitTxConfirmed
  , balanceTx
  , balanceTxWithConstraints
  , mkUnbalancedTx
  , signTransaction
  , submit
  ) as Effect
import TrustlessSidechain.Effects.Util (mapError)
import TrustlessSidechain.Error
  ( OffchainError(BalanceTxError, BuildTxError)
  )
import Type.Row (type (+))

-- | Build a transaction from lookups and constraints, balance, sign and submit it to the network
-- | The function will block until the transaction is confirmed. Returns the transaction id.
-- | Please give the transaction a name as the first argument, for logging purposes.
balanceSignAndSubmit ∷
  ∀ r.
  String →
  { lookups ∷ ScriptLookups
  , constraints ∷ TxConstraints
  } →
  Run (EXCEPT OffchainError + TRANSACTION + LOG + r) TransactionHash
balanceSignAndSubmit txName { lookups, constraints } = do
  ubTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups constraints
  bsTx ← mapError BalanceTxError $ Effect.balanceTx ubTx
  signedTx ← Effect.signTransaction bsTx
  txId ← Effect.submit signedTx
  Effect.logInfo' $ "Submitted " <> txName <> " Tx: " <> show txId
  Effect.awaitTxConfirmed txId
  Effect.logInfo' $ txName <> " Tx confirmed!"

  pure txId

balanceSignAndSubmitWithoutSpendingUtxo ∷
  ∀ r.
  TransactionInput →
  String →
  { lookups ∷ ScriptLookups
  , constraints ∷ TxConstraints
  } →
  Run (EXCEPT OffchainError + TRANSACTION + LOG + r) TransactionHash
balanceSignAndSubmitWithoutSpendingUtxo
  forbiddenUtxo
  txName
  { lookups, constraints } = do
  ubTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups constraints
  let balanceTxConstraints = mustNotSpendUtxoWithOutRef forbiddenUtxo

  bsTx ← mapError BalanceTxError $ Effect.balanceTxWithConstraints ubTx
    balanceTxConstraints
  signedTx ← Effect.signTransaction bsTx
  txId ← Effect.submit signedTx
  Effect.logInfo' $ "Submitted " <> txName <> " Tx: " <> show txId
  Effect.awaitTxConfirmed txId
  Effect.logInfo' $ txName <> " Tx confirmed!"

  pure txId


txHashToByteArray :: TransactionHash -> ByteArray
txHashToByteArray txHash = unwrap $ encodeCbor txHash