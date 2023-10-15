module TrustlessSidechain.Versioning
  ( insertVersion
  , updateVersion
  , invalidateVersion
  , getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction (TransactionHash)
import Data.Array as Array
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Tx as Utils.Tx
import TrustlessSidechain.Versioning.Types as Types
import TrustlessSidechain.Versioning.Utils as Utils
import TrustlessSidechain.Versioning.V1 as V1
import TrustlessSidechain.Versioning.V2 as V2

insertVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract (Array TransactionHash)
insertVersion { sidechainParams: sp, atmsKind } version = do
  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    { sidechainParams: sp, atmsKind }
    version

  let forbiddenUtxos = mempty
  validatorsTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Map.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Map.toUnfoldable versionedPolicies
  pure (validatorsTxIds <> policiesTxIds)

invalidateVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract (Array TransactionHash)
invalidateVersion { sidechainParams: sp, atmsKind } version = do
  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    { sidechainParams: sp, atmsKind }
    version

  let forbiddenUtxos = singleton (unwrap sp).genesisUtxo

  validatorsTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Array.fromFoldable
      $ Map.keys versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Array.fromFoldable
      $ Map.keys versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

updateVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int → -- old version
  Int → -- new version
  Contract (Array TransactionHash)
updateVersion { sidechainParams: sp, atmsKind } oldVersion newVersion = do
  { versionedPolicies: oldVersionedPolicies
  , versionedValidators: oldVersionedValidators
  } ← getVersionedPoliciesAndValidators { sidechainParams: sp, atmsKind }
    oldVersion

  { versionedPolicies: newVersionedPolicies
  , versionedValidators: newVersionedValidators
  } ← getVersionedPoliciesAndValidators { sidechainParams: sp, atmsKind }
    newVersion

  let forbiddenUtxos = singleton (unwrap sp).genesisUtxo

  let
    commonPolicies = Map.intersection newVersionedPolicies oldVersionedPolicies
    commonValidators = Map.intersection newVersionedValidators
      oldVersionedValidators

  commonValidatorsTxIds ←
    traverse
      ( Utils.updateVersionTokenLookupsAndConstraints sp oldVersion newVersion >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Map.toUnfoldable commonValidators
  commonPoliciesTxIds ←
    traverse
      ( Utils.updateVersionTokenLookupsAndConstraints sp oldVersion newVersion >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Map.toUnfoldable commonPolicies

  let
    uniqueNewPolicies = Map.difference newVersionedPolicies oldVersionedPolicies
    uniqueNewValidators = Map.difference newVersionedValidators
      oldVersionedValidators

  uniqueNewValidatorsTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp newVersion >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Map.toUnfoldable uniqueNewValidators
  uniqueNewPoliciesTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp newVersion >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Map.toUnfoldable uniqueNewPolicies

  let
    uniqueOldPolicies = Map.difference oldVersionedPolicies newVersionedPolicies
    uniqueOldValidators = Map.difference oldVersionedValidators
      newVersionedValidators

  uniqueOldValidatorsTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp oldVersion >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Array.fromFoldable
      $ Map.keys uniqueOldValidators
  uniqueOldPoliciesTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp oldVersion >=>
          Utils.Tx.submitAndAwaitTx forbiddenUtxos
      )
      $ Array.fromFoldable
      $ Map.keys uniqueOldPolicies

  pure $ commonValidatorsTxIds
    <> commonPoliciesTxIds
    <> uniqueNewValidatorsTxIds
    <> uniqueNewPoliciesTxIds
    <> uniqueOldValidatorsTxIds
    <> uniqueOldPoliciesTxIds

getVersionedPoliciesAndValidators ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract
    { versionedPolicies ∷ Map.Map Types.ScriptId MintingPolicy
    , versionedValidators ∷ Map.Map Types.ScriptId Validator
    }
getVersionedPoliciesAndValidators { sidechainParams, atmsKind } version =
  case version of
    1 → V1.getVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    2 → V2.getVersionedPoliciesAndValidators sidechainParams
    _ → throwContractError ("Invalid version: " <> show version)
