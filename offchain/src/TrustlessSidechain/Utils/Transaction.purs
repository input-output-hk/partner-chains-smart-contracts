module TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  , txHashToByteArray
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints (TxConstraints)
import Data.Array as Array
import Data.ByteArray (ByteArray)
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , mkUnbalancedTx
  , signTransaction
  , submit
  , utxosAt
  ) as Effect
import TrustlessSidechain.Effects.Util (mapError)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(BalanceTxError, BuildTxError)
  )
import TrustlessSidechain.Utils.Address (getOwnWalletAddress)
import Type.Row (type (+))

-- | Build a transaction from lookups and constraints, balance, sign and submit it to the network
-- | The function will block until the transaction is confirmed. Returns the transaction id.
-- | Please give the transaction a name as the first argument, for logging purposes.
balanceSignAndSubmit ::
  forall r.
  String ->
  { lookups :: ScriptLookups
  , constraints :: TxConstraints
  } ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + LOG + r) TransactionHash
balanceSignAndSubmit txName { lookups, constraints } = do
  ubTx <- mapError BuildTxError $ Effect.mkUnbalancedTx lookups constraints

  ownAddr <- getOwnWalletAddress
  ownUtxos <- Effect.utxosAt ownAddr
  let
    ownReferenceInputs = map (\(a /\ _) -> a)
      $ Array.filter (\(_ /\ TransactionOutput { scriptRef }) -> isJust scriptRef)
      $ Map.toUnfoldable ownUtxos

  let
    balanceTxConstraints = mconcat $ map mustNotSpendUtxoWithOutRef
      ownReferenceInputs

  bsTx <- mapError BalanceTxError $ Effect.balanceTxWithConstraints ubTx
    balanceTxConstraints
  signedTx <- Effect.signTransaction bsTx
  txId <- Effect.submit signedTx
  Effect.logInfo' $ "Submitted " <> txName <> " Tx: " <> show txId
  Effect.awaitTxConfirmed txId
  Effect.logInfo' $ txName <> " Tx confirmed!"

  pure txId

txHashToByteArray :: TransactionHash -> ByteArray
txHashToByteArray txHash = unwrap $ encodeCbor txHash
