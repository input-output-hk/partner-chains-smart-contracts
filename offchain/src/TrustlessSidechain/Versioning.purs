module TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , getCommitteeSelectionPoliciesAndValidators
  , getExpectedVersionedPoliciesAndValidators
  , initializeVersion
  , insertVersion
  , invalidateVersion
  , updateVersion
  , getNativeTokenManagementPoliciesAndValidators
  ) where

import Contract.Prelude

import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value as Value
import Contract.PlutusData (fromData)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  )
import Control.Alternative (guard)
import Data.Array (fromFoldable) as Array
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Address (toAddress)
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Versioning.Types as Types
import TrustlessSidechain.Versioning.Utils
  ( getVersionOraclePolicy
  , versionOracleTokenName
  , versionOracleValidator
  )
import TrustlessSidechain.Versioning.Utils as Utils
import TrustlessSidechain.Versioning.V1 as V1
import Type.Row (type (+))

initializeVersion ::
  forall r.
  TransactionInput ->
  Run (APP + r)
    (Array TransactionHash)
initializeVersion genesisUtxo = do
  { versionedPolicies, versionedValidators } <-
    getExpectedVersionedPoliciesAndValidators genesisUtxo

  validatorsTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints genesisUtxo >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned validators"
      )
      $ List.toUnfoldable versionedValidators
  policiesTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints genesisUtxo >=>
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
insertVersion ::
  forall r.
  TransactionInput ->
  Run (APP + r)
    (Array TransactionHash)
insertVersion genesisUtxo = do
  let

    -- Filter expected, a list of ScriptId /\ a, to the ScriptId
    -- given by (expected \setdiff actual) \cap prev
    filterToScriptIds ::
      forall a.
      { expected :: List (Tuple Types.ScriptId a)
      , actual :: List (Tuple Types.ScriptId a)
      } ->
      List (Tuple Types.ScriptId a)
    filterToScriptIds { expected, actual } =
      let
        expected' = Set.fromFoldable $ map fst expected
        actual' = Set.fromFoldable $ map fst actual
        ids = Set.difference expected' actual'
      in
        List.filter (\x -> Set.member (fst x) ids) expected

  -- Debug log to help out if someone goofs on the version
  -- number.
  logDebug'
    $ "Get existing versioned policies and validators"

  { versionedPolicies: actualVersionedPolicies
  , versionedValidators: actualVersionedValidators
  } <-
    getActualVersionedPoliciesAndValidators
      genesisUtxo

  { versionedPolicies: expectedVersionedPolicies
  , versionedValidators: expectedVersionedValidators
  } <-
    getExpectedVersionedPoliciesAndValidators
      genesisUtxo

  -- Compute sets of policies / validators to insert.
  -- Should insert ones whose ScriptIds are such that they
  -- * exist among the scripts of prevVersion
  -- * have not already been inserted for version
  let
    versionedPolicies =
      filterToScriptIds
        { expected: expectedVersionedPolicies
        , actual: actualVersionedPolicies
        }
    versionedValidators =
      filterToScriptIds
        { expected: expectedVersionedValidators
        , actual: actualVersionedValidators
        }

  logInfo'
    $ "Insert validators"

  validatorsTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints genesisUtxo >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned validators"
      )
      $ List.toUnfoldable versionedValidators

  logInfo'
    $ "Insert policies"

  policiesTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints genesisUtxo >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned policies"
      )
      $ List.toUnfoldable versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

invalidateVersion ::
  forall r.
  TransactionInput ->
  Run (APP + r) (Array TransactionHash)
invalidateVersion genesisUtxo = do
  { versionedPolicies, versionedValidators } <-
    getExpectedVersionedPoliciesAndValidators
      genesisUtxo

  validatorsTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints genesisUtxo >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned validators"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst versionedValidators
  policiesTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints genesisUtxo >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned policies"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

updateVersion ::
  forall r.
  TransactionInput ->
  Run (APP + r) (Array TransactionHash)
updateVersion genesisUtxo = do
  { versionedPolicies: newVersionedPolicies
  , versionedValidators: newVersionedValidators
  } <- getExpectedVersionedPoliciesAndValidators genesisUtxo

  { versionedPolicies: oldVersionedPolicies
  , versionedValidators: oldVersionedValidators
  } <- getActualVersionedPoliciesAndValidators genesisUtxo

  oldValidatorsTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints genesisUtxo
          >=>
            Utils.Transaction.balanceSignAndSubmit
              "Update old versioned validators"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst
      $ List.filter (\x -> not (x `List.elem` newVersionedValidators))
          oldVersionedValidators

  oldPoliciesTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints genesisUtxo
          >=>
            Utils.Transaction.balanceSignAndSubmit "Update old versioned policies"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst
      $ List.filter (\x -> not (x `List.elem` newVersionedPolicies))
          oldVersionedPolicies

  newValidatorsTxIds <-
    traverse
      ( ( \(Tuple scriptId plutusScript) ->
            Utils.updateVersionLookupsAndConstraints genesisUtxo scriptId
              plutusScript
        ) >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned validators"
      )
      $ List.toUnfoldable newVersionedValidators

  newPoliciesTxIds <-
    traverse
      ( ( \(Tuple scriptId plutusScript) ->
            Utils.updateVersionLookupsAndConstraints genesisUtxo scriptId
              plutusScript
        ) >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned policies"
      )
      $ List.toUnfoldable newVersionedPolicies

  pure $ newValidatorsTxIds
    <> newPoliciesTxIds
    <> oldValidatorsTxIds
    <> oldPoliciesTxIds

-- | Get the list of "expected" validators and minting policies that should be versioned.
--
-- See Note [Expected vs actual versioned policies and validators]
getExpectedVersionedPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getExpectedVersionedPoliciesAndValidators genesisUtxo =
  V1.getVersionedPoliciesAndValidators genesisUtxo

getExpectedVersionedPoliciesAndValidatorsScriptIds ::
  { versionedPolicies :: List Types.ScriptId
  , versionedValidators :: List Types.ScriptId
  }
getExpectedVersionedPoliciesAndValidatorsScriptIds = do
  V1.getVersionedPoliciesAndValidatorsScriptIds

getCommitteeSelectionPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getCommitteeSelectionPoliciesAndValidators genesisUtxo = do
  V1.getCommitteeSelectionPoliciesAndValidators genesisUtxo

getNativeTokenManagementPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getNativeTokenManagementPoliciesAndValidators genesisUtxo = do
  V1.getNativeTokenManagementPoliciesAndValidators genesisUtxo

-- | Get the list of "actual" validators and minting policies that should be versioned.
--
-- See Note [Expected vs actual versioned policies and validators]
--
-- Used in the 'ListVersionedScripts' endpoint.
getActualVersionedPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }

getActualVersionedPoliciesAndValidators genesisUtxo =
  do
    vValidator <- versionOracleValidator genesisUtxo

    -- Get UTxOs located at the version oracle validator script address
    versionOracleValidatorAddr <- toAddress (PlutusScript.hash vValidator)
    scriptUtxos <- Effect.utxosAt versionOracleValidatorAddr
    { versionOracleCurrencySymbol } <- getVersionOraclePolicy genesisUtxo

    -- Get scripts that should be versioned
    let
      { versionedPolicies, versionedValidators } =
        getExpectedVersionedPoliciesAndValidatorsScriptIds

    -- Create Map of type 'Map ScriptHash (ScriptId, Script)' for fast retrieval
    -- of versioned scripts based on 'ScriptHash'.

    -- Get script hashes of versioned scripts that are linked to the version
    -- oracle validator.
    let
      (actualVersionedScripts :: List (Tuple Types.ScriptId PlutusScript)) =
        List.catMaybes
          $ map
              ( \(TransactionOutput { scriptRef, datum: outputDatum, amount }) ->
                  do
                    guard
                      ( Value.valueOf
                          (Asset versionOracleCurrencySymbol versionOracleTokenName)
                          amount > BigNum.fromInt 0
                      )
                    plutusScript <- case scriptRef of
                      Just (PlutusScriptRef plutusScript) -> pure plutusScript
                      _ -> Nothing
                    datum <- case outputDatum of
                      Just (OutputDatum d) -> pure d
                      _ -> Nothing
                    Types.VersionOracleDatum
                      { versionOracle: Types.VersionOracle { scriptId } } <- fromData
                      datum
                    pure $ Tuple scriptId plutusScript

              )
          $ Map.values scriptUtxos

    -- Compute 'Map ScriptId Script' based on the 'ScriptHash' of actual
    -- versioned scripts.
    let
      actualVersionedPolicies =
        List.filter
          (\(Tuple scriptId _) -> scriptId `elem` versionedPolicies)
          actualVersionedScripts

      actualVersionedValidators =
        List.filter
          (\(Tuple scriptId _) -> scriptId `elem` versionedValidators)
          actualVersionedScripts

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
