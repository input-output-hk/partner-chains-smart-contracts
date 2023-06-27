module TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE)
import Contract.PlutusData (class IsData)
import Contract.ScriptLookups (ScriptLookups, mkUnbalancedTx)
import Contract.Scripts (class ValidatorTypes)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (TxConstraints)
import Data.Bifunctor (lmap)
import TrustlessSidechain.Utils.Logging
  ( InternalError(BalanceTxError, BuildTxError)
  , OffchainError(InternalError)
  )

-- | Build a transaction from lookups and constraints, balance, sign and submit it to the network
-- | The function will block until the transaction is confirmed. Returns the transaction id.
-- | Please give the transaction a name as the first argument, for logging purposes.
balanceSignAndSubmit ∷
  ∀ (validator ∷ Type) (datum ∷ Type) (redeemer ∷ Type).
  ValidatorTypes validator datum redeemer ⇒
  IsData datum ⇒
  IsData redeemer ⇒
  String →
  ScriptLookups validator →
  TxConstraints redeemer datum →
  Contract TransactionHash
balanceSignAndSubmit txName lookups constraints = do
  ubTx ← liftedE
    ( lmap (BuildTxError >>> InternalError) <$>
        mkUnbalancedTx lookups constraints
    )
  bsTx ← liftedE
    (lmap (BalanceTxError >>> InternalError) <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ "Submitted " <> txName <> " Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ txName <> " Tx confirmed!"

  pure txId
