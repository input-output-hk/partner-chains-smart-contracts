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
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
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

  validatorsTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned validators"
      )
      $ Map.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned policies"
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

  validatorsTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned validators"
      )
      $ Array.fromFoldable
      $ Map.keys versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned policies"
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

  let
    commonPolicies = Map.intersection newVersionedPolicies oldVersionedPolicies
    commonValidators = Map.intersection newVersionedValidators
      oldVersionedValidators

  commonValidatorsTxIds ←
    traverse
      ( Utils.updateVersionTokenLookupsAndConstraints sp oldVersion newVersion >=>
          Utils.Transaction.balanceSignAndSubmit
            "Update common versioned validators"
      )
      $ Map.toUnfoldable commonValidators
  commonPoliciesTxIds ←
    traverse
      ( Utils.updateVersionTokenLookupsAndConstraints sp oldVersion newVersion >=>
          Utils.Transaction.balanceSignAndSubmit
            "Update common versioned policies"
      )
      $ Map.toUnfoldable commonPolicies

  let
    uniqueNewPolicies = Map.difference newVersionedPolicies oldVersionedPolicies
    uniqueNewValidators = Map.difference newVersionedValidators
      oldVersionedValidators

  uniqueNewValidatorsTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned validators"
      )
      $ Map.toUnfoldable uniqueNewValidators
  uniqueNewPoliciesTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned policies"
      )
      $ Map.toUnfoldable uniqueNewPolicies

  let
    uniqueOldPolicies = Map.difference oldVersionedPolicies newVersionedPolicies
    uniqueOldValidators = Map.difference oldVersionedValidators
      newVersionedValidators

  uniqueOldValidatorsTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp oldVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update old versioned validators"
      )
      $ Array.fromFoldable
      $ Map.keys uniqueOldValidators
  uniqueOldPoliciesTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp oldVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update old versioned policies"
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
