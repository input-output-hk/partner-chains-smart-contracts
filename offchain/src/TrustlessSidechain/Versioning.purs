module TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , getCommitteeSelectionPoliciesAndValidators
  , getCheckpointPoliciesAndValidators
  , getExpectedVersionedPoliciesAndValidators
  , initializeVersion
  , insertVersion
  , invalidateVersion
  , mintVersionInitTokens
  , updateVersion
  , getFuelPoliciesAndValidators
  , getDsPoliciesAndValidators
  , getMerkleRootPoliciesAndValidators
  , getNativeTokenManagementPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.PlutusData (RedeemerDatum(RedeemerDatum), toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Transaction
  ( TransactionHash
  )
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.OutputDatum (outputDatumDatum)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Cardano.Types.Value as Value
import Data.Array (fromFoldable) as Array
import JS.BigInt as BigInt
import Data.List (List)
import Contract.Numeric.BigNum as BigNum
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.InitSidechain.Types (InitTokenRedeemer(MintInitToken))
import TrustlessSidechain.InitSidechain.Utils (initTokenCurrencyInfo)
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
import Type.Row (type (+))
import Cardano.Types.Mint as Mint
import Cardano.Types.Int as Int

-- | Mint multiple version oracle init tokens.  Exact amount minted depends on
-- | protocol version.
mintVersionInitTokens ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mintVersionInitTokens { sidechainParams, atmsKind } version = do
  { versionedPolicies, versionedValidators } ←
    getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind }
      version

  let
    amount = Int.fromInt
      (List.length versionedPolicies + List.length versionedValidators)

  { mintingPolicy, currencySymbol } ← initTokenCurrencyInfo sidechainParams

  pure
    { lookups: Lookups.plutusMintingPolicy mintingPolicy
    , constraints:
        Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ toData MintInitToken)
          (Mint.singleton currencySymbol versionOracleInitTokenName amount)
    }

initializeVersion ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Run (APP + r)
    (Array TransactionHash)
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

-- Note [Supporting version insertion beyond version 2]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- insertVersion is very brittle and only provides
-- reasonable behavior in the current situation, where only
-- versions 1 and 2 are supported. Therefore, the only version
-- argument passed here that will succeed and do what the function
-- says is 2.
-- We considered making this more robust as part of ETCM-6883, since
-- clearly it is the intention to support a greater range of versions
-- in trustless-sidechain. However, the type and documentation for
-- version numbers doesn't really convey what is allowed, for example
-- whether we intend to support version increments of size greater than
-- 1. For example, should we support a case where scripts A and B are
-- initialized to version 1, script B is updated to version 2, and
-- then scripts A and B are updated to version 3?
-- A fairly flexible solution for supporting such cases is to
-- call getActualVersionedPoliciesAndValidators repeatedly with version
-- numbers version - 1, version -2 ... until the function returns empty
-- lists. Then, scripts will be updated to the target version if they
-- appear in at least one previous verions, meaning if there is a currently
-- valid version of the script for at least some previous version.
-- See also
-- https://github.com/input-output-hk/trustless-sidechain/pull/756#discussion_r1551342345

-- | Insert scripts for the supplied version, only
-- | for features already existing and still valid in version - 1.
-- | This assumes versions increase in increments of 1, so that
-- | the supplied `version` argument is equal to the latest version plus 1.
-- | If a script is already present for `version`, do not try to re-insert it.
insertVersion ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Run (APP + r)
    (Array TransactionHash)
insertVersion { sidechainParams, atmsKind } version = do
  let
    prevVersion = version - 1

    -- Filter expected, a list of ScriptId /\ a, to the ScriptId
    -- given by (expected \setdiff actual) \cap prev
    filterToScriptIds ∷
      ∀ a.
      { expected ∷ List (Tuple Types.ScriptId a)
      , actual ∷ List (Tuple Types.ScriptId a)
      , prev ∷ List (Tuple Types.ScriptId a)
      } →
      List (Tuple Types.ScriptId a)
    filterToScriptIds { expected, actual, prev } =
      let
        expected' = Set.fromFoldable $ map fst expected
        actual' = Set.fromFoldable $ map fst actual
        prev' = Set.fromFoldable $ map fst prev
        ids = Set.intersection prev' (Set.difference expected' actual')
      in
        List.filter (\x → Set.member (fst x) ids) expected

  -- Debug log to help out if someone goofs on the version
  -- number.
  logDebug'
    $ "Get existing versioned policies and validators for version"
    <> show prevVersion

  { versionedPolicies: prevVersionedPolicies
  , versionedValidators: prevVersionedValidators
  } ←
    getActualVersionedPoliciesAndValidators
      { sidechainParams, atmsKind }
      prevVersion

  { versionedPolicies: actualVersionedPolicies
  , versionedValidators: actualVersionedValidators
  } ←
    getActualVersionedPoliciesAndValidators
      { sidechainParams, atmsKind }
      version

  { versionedPolicies: expectedVersionedPolicies
  , versionedValidators: expectedVersionedValidators
  } ←
    getExpectedVersionedPoliciesAndValidators
      { sidechainParams, atmsKind }
      version

  -- Compute sets of policies / validators to insert.
  -- Should insert ones whose ScriptIds are such that they
  -- * exist among the scripts of prevVersion
  -- * have not already been inserted for version
  let
    versionedPolicies =
      filterToScriptIds
        { expected: expectedVersionedPolicies
        , actual: actualVersionedPolicies
        , prev: prevVersionedPolicies
        }
    versionedValidators =
      filterToScriptIds
        { expected: expectedVersionedValidators
        , actual: actualVersionedValidators
        , prev: prevVersionedValidators
        }

  logInfo'
    $ "Insert validators for version"
    <> show version

  validatorsTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned validators"
      )
      $ List.toUnfoldable versionedValidators

  logInfo'
    $ "Insert policies for version"
    <> show version

  policiesTxIds ←
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned policies"
      )
      $ List.toUnfoldable versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

invalidateVersion ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Run (APP + r) (Array TransactionHash)
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
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int → -- old version
  Int → -- new version
  Run (APP + r) (Array TransactionHash)
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
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getExpectedVersionedPoliciesAndValidators { sidechainParams, atmsKind } version =
  case version of
    1 → V1.getVersionedPoliciesAndValidators { sidechainParams, atmsKind }
    2 → V2.getVersionedPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

getCommitteeSelectionPoliciesAndValidators ∷
  ∀ r.
  ATMSKinds →
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getCommitteeSelectionPoliciesAndValidators atmsKind sidechainParams version = do
  case version of
    1 → V1.getCommitteeSelectionPoliciesAndValidators atmsKind sidechainParams
    2 → V2.getCommitteeSelectionPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

getCheckpointPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getCheckpointPoliciesAndValidators sidechainParams version = do
  case version of
    1 → V1.getCheckpointPoliciesAndValidators sidechainParams
    2 → V2.getCheckpointPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

getNativeTokenManagementPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple Types.ScriptId Validator)
    }
getNativeTokenManagementPoliciesAndValidators sidechainParams version = do
  case version of
    1 → V1.getNativeTokenManagementPoliciesAndValidators sidechainParams
    2 → V2.getNativeTokenManagementPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

-- | Get the list of "actual" validators and minting policies that should be versioned.
--
-- See Note [Expected vs actual versioned policies and validators]
--
-- Used in the 'ListVersionedScripts' endpoint.
getActualVersionedPoliciesAndValidators ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Int →
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }

getActualVersionedPoliciesAndValidators { sidechainParams, atmsKind } version =
  do
    vValidator ← versionOracleValidator sidechainParams

    -- Get UTxOs located at the version oracle validator script address
    versionOracleValidatorAddr ← toAddress (PlutusScript.hash vValidator)
    scriptUtxos ← Effect.utxosAt versionOracleValidatorAddr

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
              ( \(TransactionOutput { scriptRef }) → case scriptRef of
                    Just (PlutusScriptRef plutusScript) -> Just $ PlutusScript.hash plutusScript
                    _ -> Nothing
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

-- | Get versioned policies and validators for
-- | FUEL minting and burning.
getFuelPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getFuelPoliciesAndValidators sidechainParams version =
  case version of
    1 → V1.getFuelPoliciesAndValidators sidechainParams
    2 → V2.getFuelPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

-- | Get versioned policies and validators for
-- | Ds* script ids.
getDsPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getDsPoliciesAndValidators sidechainParams version =
  case version of
    1 → V1.getDsPoliciesAndValidators sidechainParams
    2 → V2.getDsPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

getMerkleRootPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Int →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getMerkleRootPoliciesAndValidators sidechainParams version =
  case version of
    1 → V1.getMerkleRootPoliciesAndValidators sidechainParams
    2 → V2.getMerkleRootPoliciesAndValidators sidechainParams
    _ → throw $ GenericInternalError ("Invalid version: " <> show version)

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
