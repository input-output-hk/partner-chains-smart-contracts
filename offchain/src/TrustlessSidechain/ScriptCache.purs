-- | `TrustlessSidechain.ScriptCache` provides a simple script caching
-- | mechanism.  This is required for sidechain initialization, when we need to
-- | cache multiple minting policies before we attempt minting initial sidechain
-- | tokens.
module TrustlessSidechain.ScriptCache
  ( getScriptRefUtxo
  ) where

import Contract.Prelude hiding (unit)

import Cardano.Types.PlutusData (unit)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( ScriptRef
  , TransactionInput(TransactionInput)
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import Run.Except as Run
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , mkUnbalancedTx
  , signTransaction
  , submit
  , utxosAt
  ) as Effect
import TrustlessSidechain.Effects.Util (mapError)
import TrustlessSidechain.Error
  ( OffchainError(GenericInternalError, BuildTxError, BalanceTxError)
  )
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , toAddress
  )
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(ScriptCache)
  )
import Type.Row (type (+))

getScriptCacheValidator ::
  forall r.
  PaymentPubKeyHash ->
  Run (EXCEPT OffchainError + r) PlutusScript
getScriptCacheValidator (PaymentPubKeyHash pkh) =
  mkValidatorWithParams ScriptCache [ toData pkh ]

getScriptRefUtxo ::
  forall r.
  TransactionInput ->
  ScriptRef ->
  Run (APP + r) (TransactionInput /\ TransactionOutput)
getScriptRefUtxo genesisUtxo scriptRef = do
  pkh <- getOwnPaymentPubKeyHash
  scriptCacheValidatorHash <- PlutusScript.hash <$> getScriptCacheValidator pkh

  valAddr <- toAddress scriptCacheValidatorHash

  scriptCacheUtxos <- Effect.utxosAt valAddr

  let
    correctOutput
      ( _ /\ TransactionOutput
          { scriptRef: Just scriptRef' }
      ) = scriptRef' == scriptRef
    correctOutput _ = false

  case find correctOutput (Map.toUnfoldable scriptCacheUtxos :: Array _) of
    Just scriptRefUtxo -> pure scriptRefUtxo
    Nothing -> createScriptRefUtxo genesisUtxo scriptRef

createScriptRefUtxo ::
  forall r.
  TransactionInput ->
  ScriptRef ->
  Run (APP + r) (TransactionInput /\ TransactionOutput)
createScriptRefUtxo genesisUtxo scriptRef = do
  pkh <- getOwnPaymentPubKeyHash
  scriptCacheValidatorHash <- PlutusScript.hash <$> getScriptCacheValidator pkh

  let
    constraints :: TxConstraints
    constraints = Constraints.mustPayToScriptWithScriptRef
      scriptCacheValidatorHash
      unit
      DatumInline
      scriptRef
      (Value.lovelaceValueOf $ BigNum.fromInt 1) -- minimum possible ada

    lookups :: Lookups.ScriptLookups
    lookups = mempty

    balanceTxConstraints :: BalanceTxConstraints.BalanceTxConstraintsBuilder
    balanceTxConstraints =
      BalanceTxConstraints.mustNotSpendUtxoWithOutRef genesisUtxo

  ubTx <- mapError BuildTxError $ Effect.mkUnbalancedTx lookups constraints
  bsTx <- mapError BalanceTxError $ Effect.balanceTxWithConstraints ubTx
    balanceTxConstraints
  signedTx <- Effect.signTransaction bsTx
  versioningScriptRefUtxoTxId <- Effect.submit signedTx
  Effect.logInfo' $ "Submitted create script ref utxo: "
    <> show versioningScriptRefUtxoTxId
  Effect.awaitTxConfirmed versioningScriptRefUtxoTxId

  valAddr <- toAddress scriptCacheValidatorHash

  scriptCacheUtxos <- Effect.utxosAt valAddr

  let
    correctOutput
      ( TransactionInput x /\ TransactionOutput
          { scriptRef: Just scriptRef' }
      ) =
      x.transactionId == versioningScriptRefUtxoTxId
        && scriptRef'
        == scriptRef
    correctOutput _ = false

  txInput /\ txOutput <-
    Run.note
      ( GenericInternalError
          $ "Could not find unspent output with correct "
          <> "script ref locked at script cache address"
      )
      $ find correctOutput (Map.toUnfoldable scriptCacheUtxos :: Array _)

  pure (txInput /\ txOutput)
