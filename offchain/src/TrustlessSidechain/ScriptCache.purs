-- | `TrustlessSidechain.ScriptCache` provides a simple script caching
-- | mechanism.  This is required for sidechain initialization, when we need to
-- | cache multiple minting policies before we attempt minting initial sidechain
-- | tokens.
module TrustlessSidechain.ScriptCache
  ( getPolicyScriptRefUtxo
  , getValidatorScriptRefUtxo
  , ScriptRefUtxo
  ) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash(PaymentPubKeyHash)
  )
import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.PlutusData (toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy, NativeMintingPolicy)
  , Validator(Validator)
  , validatorHash
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef, NativeScriptRef)
  , TransactionInput(TransactionInput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt as BigInt
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
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
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

getScriptCacheValidator ∷
  ∀ r.
  PaymentPubKeyHash →
  Run (EXCEPT OffchainError + r) Validator
getScriptCacheValidator (PaymentPubKeyHash pkh) =
  mkValidatorWithParams ScriptCache [ toData pkh ]

getScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  ScriptRef →
  Run (APP + r) (TransactionInput /\ TransactionOutputWithRefScript)
getScriptRefUtxo (SidechainParams sp) scriptRef = do
  pkh ← getOwnPaymentPubKeyHash
  scriptCacheValidatorHash ← validatorHash <$> getScriptCacheValidator pkh

  valAddr ← toAddress scriptCacheValidatorHash

  scriptCacheUtxos ← Effect.utxosAt valAddr

  let
    correctOutput
      ( _ /\ TransactionOutputWithRefScript
          { scriptRef: Just scriptRef' }
      ) = scriptRef' == scriptRef
    correctOutput _ = false

  case find correctOutput (Map.toUnfoldable scriptCacheUtxos ∷ Array _) of
    Just scriptRefUtxo → pure scriptRefUtxo
    Nothing → createScriptRefUtxo (SidechainParams sp) scriptRef

createScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  ScriptRef →
  Run (APP + r) (TransactionInput /\ TransactionOutputWithRefScript)
createScriptRefUtxo (SidechainParams sp) scriptRef = do
  pkh ← getOwnPaymentPubKeyHash
  scriptCacheValidatorHash ← validatorHash <$> getScriptCacheValidator pkh

  let
    constraints ∷ TxConstraints Void Void
    constraints = Constraints.mustPayToScriptWithScriptRef
      scriptCacheValidatorHash
      unitDatum
      DatumInline
      scriptRef
      (Value.lovelaceValueOf $ BigInt.fromInt 1) -- minimum possible ada

    lookups ∷ Lookups.ScriptLookups Void
    lookups = mempty

    balanceTxConstraints ∷ BalanceTxConstraints.BalanceTxConstraintsBuilder
    balanceTxConstraints =
      BalanceTxConstraints.mustNotSpendUtxoWithOutRef sp.genesisUtxo

  ubTx ← mapError BuildTxError $ Effect.mkUnbalancedTx lookups constraints
  bsTx ← mapError BalanceTxError $ Effect.balanceTxWithConstraints ubTx
    balanceTxConstraints
  signedTx ← Effect.signTransaction bsTx
  versioningScriptRefUtxoTxId ← Effect.submit signedTx
  Effect.logInfo' $ "Submitted create script ref utxo: "
    <> show versioningScriptRefUtxoTxId
  Effect.awaitTxConfirmed versioningScriptRefUtxoTxId

  valAddr ← toAddress scriptCacheValidatorHash

  scriptCacheUtxos ← Effect.utxosAt valAddr

  let
    correctOutput
      ( TransactionInput x /\ TransactionOutputWithRefScript
          { scriptRef: Just scriptRef' }
      ) =
      x.transactionId == versioningScriptRefUtxoTxId
        && scriptRef'
        == scriptRef
    correctOutput _ = false

  txInput /\ txOutput ←
    Run.note
      ( GenericInternalError
          $ "Could not find unspent output with correct "
          <> "script ref locked at script cache address"
      )
      $ find correctOutput (Map.toUnfoldable scriptCacheUtxos ∷ Array _)

  pure (txInput /\ txOutput)

getValidatorScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  Validator →
  Run (APP + r) (TransactionInput /\ TransactionOutputWithRefScript)
getValidatorScriptRefUtxo sp (Validator script) = getScriptRefUtxo sp
  (PlutusScriptRef script)

getPolicyScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  MintingPolicy →
  Run (APP + r) (TransactionInput /\ TransactionOutputWithRefScript)
getPolicyScriptRefUtxo sp (PlutusMintingPolicy script) = getScriptRefUtxo sp
  (PlutusScriptRef script)
getPolicyScriptRefUtxo sp (NativeMintingPolicy script) = getScriptRefUtxo sp
  (NativeScriptRef script)

type ScriptRefUtxo = TransactionInput /\ TransactionOutputWithRefScript
