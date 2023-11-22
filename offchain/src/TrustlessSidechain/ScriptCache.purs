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
  , getNetworkId
  , validatorHashEnterpriseAddress
  )
import Contract.BalanceTxConstraints as BalanceTxConstraints
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE)
import Contract.PlutusData (PlutusData, toData, unitDatum)
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
  , awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Error (InternalError(InvalidScript))
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )

getScriptCacheValidator ∷ PaymentPubKeyHash → Contract Validator
getScriptCacheValidator (PaymentPubKeyHash pkh) =
  mkValidatorWithParams RawScripts.rawScriptCache [ toData pkh ]

getScriptRefUtxo ∷
  SidechainParams →
  ScriptRef →
  Contract (TransactionInput /\ TransactionOutputWithRefScript)
getScriptRefUtxo (SidechainParams sp) scriptRef = do
  pkh ← getOwnPaymentPubKeyHash
  scriptCacheValidatorHash ← validatorHash <$> getScriptCacheValidator pkh

  netId ← getNetworkId
  valAddr ← liftContractM
    (show (InvalidScript "script cache enterprise address"))
    (validatorHashEnterpriseAddress netId scriptCacheValidatorHash)

  scriptCacheUtxos ← utxosAt valAddr

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
  SidechainParams →
  ScriptRef →
  Contract (TransactionInput /\ TransactionOutputWithRefScript)
createScriptRefUtxo (SidechainParams sp) scriptRef = do
  pkh ← getOwnPaymentPubKeyHash
  scriptCacheValidatorHash ← validatorHash <$> getScriptCacheValidator pkh

  let
    constraints ∷ TxConstraints Unit Unit
    constraints = Constraints.mustPayToScriptWithScriptRef
      scriptCacheValidatorHash
      unitDatum
      DatumInline
      scriptRef
      (Value.lovelaceValueOf $ BigInt.fromInt 1) -- minimum possible ada

    lookups ∷ Lookups.ScriptLookups PlutusData
    lookups = mempty

    balanceTxConstraints ∷ BalanceTxConstraints.BalanceTxConstraintsBuilder
    balanceTxConstraints =
      BalanceTxConstraints.mustNotSpendUtxoWithOutRef sp.genesisUtxo

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE
    (balanceTxWithConstraints ubTx balanceTxConstraints)
  signedTx ← signTransaction bsTx
  versioningScriptRefUtxoTxId ← submit signedTx
  logInfo' $ "Submitted create script ref utxo: "
    <> show versioningScriptRefUtxoTxId
  awaitTxConfirmed versioningScriptRefUtxoTxId

  netId ← getNetworkId
  valAddr ← liftContractM
    (show (InvalidScript "script cache enterprise address"))
    (validatorHashEnterpriseAddress netId scriptCacheValidatorHash)

  scriptCacheUtxos ← utxosAt valAddr

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
    liftContractM
      "Could not find unspent output with correct script ref locked at script cache address"
      $ find correctOutput (Map.toUnfoldable scriptCacheUtxos ∷ Array _)

  pure (txInput /\ txOutput)

getValidatorScriptRefUtxo ∷
  SidechainParams →
  Validator →
  Contract (TransactionInput /\ TransactionOutputWithRefScript)
getValidatorScriptRefUtxo sp (Validator script) = getScriptRefUtxo sp
  (PlutusScriptRef script)

getPolicyScriptRefUtxo ∷
  SidechainParams →
  MintingPolicy →
  Contract (TransactionInput /\ TransactionOutputWithRefScript)
getPolicyScriptRefUtxo sp (PlutusMintingPolicy script) = getScriptRefUtxo sp
  (PlutusScriptRef script)
getPolicyScriptRefUtxo sp (NativeMintingPolicy script) = getScriptRefUtxo sp
  (NativeScriptRef script)

type ScriptRefUtxo = TransactionInput /\ TransactionOutputWithRefScript
