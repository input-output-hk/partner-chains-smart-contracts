module TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  , balanceSignAndSubmitWithoutSpendingUtxo
  , txHashToByteArray
  , balanceAndWriteWithoutSpendingUtxo
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Transaction (Transaction)
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef)
import Contract.CborBytes (cborBytesToHex)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  )
import Contract.TxConstraints (TxConstraints)
import Data.Argonaut.Core as J
import Data.ByteArray (ByteArray)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Run (AFF, EFFECT, Run)
import Run as Run
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

balanceAndWriteWithoutSpendingUtxo ∷
  ∀ r.
  TransactionInput →
  { lookups ∷ ScriptLookups
  , constraints ∷ TxConstraints
  } →
  String →
  Run (EXCEPT OffchainError + EFFECT + TRANSACTION + AFF + LOG + r) Unit
balanceAndWriteWithoutSpendingUtxo
  forbiddenUtxo
  { lookups, constraints }
  outputFile = do
  ubTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups constraints
  let balanceTxConstraints = mustNotSpendUtxoWithOutRef forbiddenUtxo

  tx ← mapError BalanceTxError $
    Effect.balanceTxWithConstraints ubTx
      balanceTxConstraints

  Run.liftAff $ writeTextFile UTF8 outputFile
    $ J.stringifyWithIndent 4
    $ J.fromObject
    $ Object.fromFoldable
        [ "type" /\ J.fromString "Unwitnessed Tx BabbageEra"
        , "description" /\ J.fromString "Ledger Cddl Format"
        , "cborHex" /\ J.fromString (serialiseTx tx)
        ]

  where
  serialiseTx ∷ Transaction → String
  serialiseTx = encodeCbor >>> cborBytesToHex

txHashToByteArray ∷ TransactionHash → ByteArray
txHashToByteArray txHash = unwrap $ encodeCbor txHash
