module TrustlessSidechain.Versioning
  ( initializeVersion
  , insertVersion
  , updateVersion
  , invalidateVersion
  , getExpectedVersionedPoliciesAndValidators
  , getActualVersionedPoliciesAndValidators
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
import Contract.Scripts
  ( MintingPolicy
  , ScriptHash
  , Validator
  , validatorHash
  )
import Contract.Transaction
  ( TransactionHash
  , TransactionOutput(..)
  , TransactionOutputWithRefScript(..)
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Array (fromFoldable) as Array
import Data.BigInt as BigInt
import Data.List (List)
import Data.List as List
import Data.Map as Map
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenRedeemer(MintInitToken)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( initTokenCurrencyInfo
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toAddress)
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Versioning.Types (toScriptHash)
import TrustlessSidechain.Versioning.Types as Types
import TrustlessSidechain.Versioning.Utils
  ( versionOracleInitTokenName
  , versionOracleValidator
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
    getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind }
      version

  let
    amount = BigInt.fromInt
      (List.length versionedPolicies + List.length versionedValidators)

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
  { versionedPolicies, versionedValidators } ←
    getExpectedVersionedPoliciesAndValidators
      { sidechainParams, atmsKind }
      version

  validatorsTxIds ←
    traverse
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned validators"
      )
      $ List.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned policies"
      )
      $ List.toUnfoldable versionedPolicies
  pure (validatorsTxIds <> policiesTxIds)

insertVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract (Array TransactionHash)
insertVersion { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ←
    getExpectedVersionedPoliciesAndValidators
      { sidechainParams, atmsKind }
      version

  validatorsTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned validators"
      )
      $ List.toUnfoldable versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned policies"
      )
      $ List.toUnfoldable versionedPolicies
  pure (validatorsTxIds <> policiesTxIds)

invalidateVersion ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract (Array TransactionHash)
invalidateVersion { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ←
    getExpectedVersionedPoliciesAndValidators
      { sidechainParams, atmsKind }
      version

  validatorsTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned validators"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst versionedValidators
  policiesTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned policies"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst versionedPolicies

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
  } ← getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    oldVersion

  { versionedPolicies: newVersionedPolicies
  , versionedValidators: newVersionedValidators
  } ← getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    newVersion

  newValidatorsTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned validators"
      )
      $ List.toUnfoldable newVersionedValidators
  newPoliciesTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned policies"
      )
      $ List.toUnfoldable newVersionedPolicies

  oldValidatorsTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams oldVersion
          >=>
            Utils.Transaction.balanceSignAndSubmit
              "Update old versioned validators"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst oldVersionedValidators
  oldPoliciesTxIds ←
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams oldVersion
          >=>
            Utils.Transaction.balanceSignAndSubmit "Update old versioned policies"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst oldVersionedPolicies

  pure $ newValidatorsTxIds
    <> newPoliciesTxIds
    <> oldValidatorsTxIds
    <> oldPoliciesTxIds

-- | Get the list of "expected" validators and minting policies that should be versioned.
--
-- See Note [Expected vs actual versioned policies and validators]
getExpectedVersionedPoliciesAndValidators ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract
    { versionedPolicies ∷ List (Tuple Types.ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple Types.ScriptId Validator)
    }
getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind } version =
  case version of
    1 → V1.getVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    2 → V2.getVersionedPoliciesAndValidators sidechainParams
    _ → throwContractError ("Invalid version: " <> show version)

-- | Get the list of "actual" validators and minting policies that should be versioned.
--
-- See Note [Expected vs actual versioned policies and validators]
--
-- Used in the 'ListVersionedScripts' endpoint.
getActualVersionedPoliciesAndValidators ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Contract
    { versionedPolicies ∷ List (Tuple Types.ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple Types.ScriptId Validator)
    }

getActualVersionedPoliciesAndValidators { sidechainParams, atmsKind } version =
  do
    vValidator ← versionOracleValidator sidechainParams

    -- Get UTxOs located at the version oracle validator script address
    versionOracleValidatorAddr ← toAddress (validatorHash vValidator)
    scriptUtxos ← utxosAt versionOracleValidatorAddr

    -- Get scripts that should be versioned
    { versionedPolicies, versionedValidators } ←
      getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind }
        version

    -- Create Map of type 'Map ScriptHash (ScriptId, Script)' for fast retrieval
    -- of versioned scripts based on 'ScriptHash'.
    let
      versionedPoliciesIndexedByHash =
        Map.fromFoldable
          $ map (\t@(Tuple _ script) → toScriptHash script /\ t)
          $ versionedPolicies

      versionedValidatorsIndexedByHash =
        Map.fromFoldable
          $ map (\t@(Tuple _ script) → toScriptHash script /\ t)
          $ versionedValidators

    -- Get script hashes of versioned scripts that are linked to the version
    -- oracle validator.
    let
      (actualVersionedScriptHashes ∷ List ScriptHash) =
        List.catMaybes
          $ map
              ( \(TransactionOutputWithRefScript { output }) →
                  case output of
                    TransactionOutput
                      { referenceScript
                      } → referenceScript
              )
          $ Map.values scriptUtxos

    -- Compute 'Map ScriptId Script' based on the 'ScriptHash' of actual
    -- versioned scripts.
    let
      actualVersionedPolicies =
        List.catMaybes $
          map (\scriptHash → Map.lookup scriptHash versionedPoliciesIndexedByHash)
            actualVersionedScriptHashes

      actualVersionedValidators =
        List.catMaybes $
          map
            (\scriptHash → Map.lookup scriptHash versionedValidatorsIndexedByHash)
            actualVersionedScriptHashes

    pure
      { versionedPolicies: actualVersionedPolicies
      , versionedValidators: actualVersionedValidators
      }

-- Note [Expected vs actual versioned policies and validators]
--
-- In the codebase, we define/hardcode the list of minting policies and
-- validators that _should_ be versioned. When we initialize a sidechain, we
-- insert all those scripts into the versioning system. Additionally, new
-- versioned scripts can be added during the sidechain's lifetime. This means
-- that all these scripts are being cached as reference scripts on the
-- sidechain. However, the initialization process is not atomic, as each script
-- is cached in a separate transaction. If one or more transactions fail, the
-- initialization process fails. We then get in a situation where some of the
-- scripts have already been cached while others have not.
--
-- This brings up the notion of minting policies and validators that are
-- _actually_ versioned vs _should_ be versioned.
