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
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Logging (Location)
import TrustlessSidechain.Utils.Tx as Utils.Tx
import TrustlessSidechain.Versioning.Types as Types
import TrustlessSidechain.Versioning.Utils as Utils
import TrustlessSidechain.Versioning.V1 as V1
import TrustlessSidechain.Versioning.V2 as V2

insertVersion ∷
  SidechainParams →
  Int →
  Contract (Array TransactionHash)
insertVersion sp version = do
  let
    loc = mkLoc "insertVersion"

  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    sp
    version

  validatorsTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Map.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Map.toUnfoldable versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

invalidateVersion ∷
  SidechainParams →
  Int →
  Contract (Array TransactionHash)
invalidateVersion sp version = do
  let
    loc = mkLoc "invalidateVersion"

  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    sp
    version

  validatorsTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Array.fromFoldable
      $ Map.keys versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp version >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Array.fromFoldable
      $ Map.keys versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

updateVersion ∷
  SidechainParams →
  Int → -- old version
  Int → -- new version
  Contract (Array TransactionHash)
updateVersion sp oldVersion newVersion = do
  let
    loc = mkLoc "updateVersion"

  { versionedPolicies: oldVersionedPolicies
  , versionedValidators: oldVersionedValidators
  } ← getVersionedPoliciesAndValidators sp oldVersion

  { versionedPolicies: newVersionedPolicies
  , versionedValidators: newVersionedValidators
  } ← getVersionedPoliciesAndValidators sp newVersion

  let
    commonPolicies = Map.intersection newVersionedPolicies oldVersionedPolicies
    commonValidators = Map.intersection newVersionedValidators
      oldVersionedValidators

  commonValidatorsTxIds ←
    traverse
      ( Utils.updateVersionTokenLookupsAndConstraints sp oldVersion newVersion >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Map.toUnfoldable commonValidators
  commonPoliciesTxIds ←
    traverse
      ( Utils.updateVersionTokenLookupsAndConstraints sp oldVersion newVersion >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Map.toUnfoldable commonPolicies

  let
    uniqueNewPolicies = Map.difference newVersionedPolicies oldVersionedPolicies
    uniqueNewValidators = Map.difference newVersionedValidators
      oldVersionedValidators

  uniqueNewValidatorsTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp newVersion >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Map.toUnfoldable uniqueNewValidators
  uniqueNewPoliciesTxIds ←
    traverse
      ( Utils.insertVersionTokenLookupsAndConstraints sp newVersion >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Map.toUnfoldable uniqueNewPolicies

  let
    uniqueOldPolicies = Map.difference oldVersionedPolicies newVersionedPolicies
    uniqueOldValidators = Map.difference oldVersionedValidators
      newVersionedValidators

  uniqueOldValidatorsTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp oldVersion >=>
          Utils.Tx.submitAndAwaitTx loc
      )
      $ Array.fromFoldable
      $ Map.keys uniqueOldValidators
  uniqueOldPoliciesTxIds ←
    traverse
      ( Utils.invalidateVersionTokenLookupsAndConstraints sp oldVersion >=>
          Utils.Tx.submitAndAwaitTx loc
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
  SidechainParams →
  Int →
  Contract
    { versionedPolicies ∷ Map.Map Types.ScriptId MintingPolicy
    , versionedValidators ∷ Map.Map Types.ScriptId Validator
    }
getVersionedPoliciesAndValidators sp version = case version of
  1 → V1.getVersionedPoliciesAndValidators sp
  2 → V2.getVersionedPoliciesAndValidators sp
  _ → throwContractError ("Invalid version: " <> show version)

mkLoc ∷ String → Location
mkLoc fun = { mod: "Versioning", fun }
