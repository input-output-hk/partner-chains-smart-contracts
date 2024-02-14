module TrustlessSidechain.Versioning
  ( initializeVersion
  , insertVersion
  , updateVersion
  , invalidateVersion
  , getVersionedPoliciesAndValidators
  , mintVersionInitTokens
  ) where

import Contract.Prelude

import Contract.Monad (Contract, throwContractError)
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction (TransactionHash)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenRedeemer(MintInitToken)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( initTokenCurrencyInfo
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Versioning.Types as Types
import TrustlessSidechain.Versioning.Utils
  ( versionOracleInitTokenName
  )
import TrustlessSidechain.Versioning.Utils as Utils
import TrustlessSidechain.Versioning.V1 as V1
import TrustlessSidechain.Versioning.V2 as V2

-- | Mint multiple version oracle init tokens.  Exact amount minted depends on
-- | protocol version.
mintVersionInitTokens ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mintVersionInitTokens { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ←
    getVersionedPoliciesAndValidators { sidechainParams, atmsKind } version

  let
    amount = BigInt.fromInt
      (Map.size versionedPolicies + Map.size versionedValidators)

  { mintingPolicy, currencySymbol } ← initTokenCurrencyInfo sidechainParams

  pure
    { lookups: Lookups.mintingPolicy mintingPolicy
    , constraints:
        Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData MintInitToken)
          (Value.singleton currencySymbol versionOracleInitTokenName amount)
    }

initializeVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract (Array TransactionHash)
initializeVersion { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    { sidechainParams, atmsKind }
    version

  validatorsTxIds ←
    traverse
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned validators"
      )
      $ Map.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned policies"
      )
      $ Map.toUnfoldable versionedPolicies
  pure (validatorsTxIds <> policiesTxIds)

insertVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract (Array TransactionHash)
insertVersion { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    { sidechainParams, atmsKind }
    version

  validatorsTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned validators"
      )
      $ Map.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
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
invalidateVersion { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ← getVersionedPoliciesAndValidators
    { sidechainParams, atmsKind }
    version

  validatorsTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned validators"
      )
      $ Array.fromFoldable
      $ Map.keys versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams version >=>
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
updateVersion { sidechainParams, atmsKind } oldVersion newVersion = do
  { versionedPolicies: oldVersionedPolicies
  , versionedValidators: oldVersionedValidators
  } ← getVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    oldVersion

  { versionedPolicies: newVersionedPolicies
  , versionedValidators: newVersionedValidators
  } ← getVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    newVersion

  let
    commonPolicies = Map.intersection newVersionedPolicies oldVersionedPolicies
    commonValidators = Map.intersection newVersionedValidators
      oldVersionedValidators

  commonValidatorsTxIds ←
    traverse
      ( Utils.updateVersionLookupsAndConstraints sidechainParams oldVersion
          newVersion >=>
          Utils.Transaction.balanceSignAndSubmit
            "Update common versioned validators"
      )
      $ Map.toUnfoldable commonValidators
  commonPoliciesTxIds ←
    traverse
      ( Utils.updateVersionLookupsAndConstraints sidechainParams oldVersion
          newVersion >=>
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
      ( Utils.insertVersionLookupsAndConstraints sidechainParams newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned validators"
      )
      $ Map.toUnfoldable uniqueNewValidators
  uniqueNewPoliciesTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned policies"
      )
      $ Map.toUnfoldable uniqueNewPolicies

  let
    uniqueOldPolicies = Map.difference oldVersionedPolicies newVersionedPolicies
    uniqueOldValidators = Map.difference oldVersionedValidators
      newVersionedValidators

  uniqueOldValidatorsTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams oldVersion
          >=>
            Utils.Transaction.balanceSignAndSubmit
              "Update old versioned validators"
      )
      $ Array.fromFoldable
      $ Map.keys uniqueOldValidators
  uniqueOldPoliciesTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams oldVersion
          >=>
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
