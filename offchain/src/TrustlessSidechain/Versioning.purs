module TrustlessSidechain.Versioning
  ( getActualVersionedPoliciesAndValidators
  , getCommitteeSelectionPoliciesAndValidators
  , getExpectedVersionedPoliciesAndValidators
  , initializeVersion
  , insertVersion
  , invalidateVersion
  , mintVersionInitTokens
  , updateVersion
  , getNativeTokenManagementPoliciesAndValidators
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Contract.PlutusData (RedeemerDatum(RedeemerDatum), toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Data.Array (fromFoldable) as Array
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Env (Env, READER)
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
import TrustlessSidechain.Versioning.Types as Types
import TrustlessSidechain.Versioning.Utils
  ( versionOracleInitTokenName
  , versionOracleValidator
  )
import TrustlessSidechain.Versioning.Utils as Utils
import TrustlessSidechain.Versioning.V1 as V1
import TrustlessSidechain.Versioning.V2 as V2
import Type.Row (type (+))

-- | Mint multiple version oracle init tokens.  Exact amount minted depends on
-- | protocol version.
mintVersionInitTokens ::
  forall r.
  SidechainParams ->
  Int ->
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
mintVersionInitTokens sidechainParams version = do
  { versionedPolicies, versionedValidators } <-
    getExpectedVersionedPoliciesAndValidators sidechainParams
      version

  let
    amount = Int.fromInt
      (List.length versionedPolicies + List.length versionedValidators)

  { mintingPolicy, currencySymbol } <- initTokenCurrencyInfo sidechainParams

  pure
    { lookups: Lookups.plutusMintingPolicy mintingPolicy
    , constraints:
        Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ toData MintInitToken)
          (Mint.singleton currencySymbol versionOracleInitTokenName amount)
    }

initializeVersion ::
  forall r.
  SidechainParams ->
  Int ->
  Run (APP + r)
    (Array TransactionHash)
initializeVersion sidechainParams version = do
  { versionedPolicies, versionedValidators } <-
    getExpectedVersionedPoliciesAndValidators sidechainParams
      version

  validatorsTxIds <-
    traverse
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned validators"
      )
      $ List.toUnfoldable versionedValidators
  policiesTxIds <-
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
insertVersion ::
  forall r.
  SidechainParams ->
  Int ->
  Run (APP + r)
    (Array TransactionHash)
insertVersion sidechainParams version = do
  let
    prevVersion = version - 1

    -- Filter expected, a list of ScriptId /\ a, to the ScriptId
    -- given by (expected \setdiff actual) \cap prev
    filterToScriptIds ::
      forall a.
      { expected :: List (Tuple Types.ScriptId a)
      , actual :: List (Tuple Types.ScriptId a)
      , prev :: List (Tuple Types.ScriptId a)
      } ->
      List (Tuple Types.ScriptId a)
    filterToScriptIds { expected, actual, prev } =
      let
        expected' = Set.fromFoldable $ map fst expected
        actual' = Set.fromFoldable $ map fst actual
        prev' = Set.fromFoldable $ map fst prev
        ids = Set.intersection prev' (Set.difference expected' actual')
      in
        List.filter (\x -> Set.member (fst x) ids) expected

  -- Debug log to help out if someone goofs on the version
  -- number.
  logDebug'
    $ "Get existing versioned policies and validators for version"
    <> show prevVersion

  { versionedPolicies: prevVersionedPolicies
  , versionedValidators: prevVersionedValidators
  } <-
    getActualVersionedPoliciesAndValidators
      sidechainParams
      prevVersion

  { versionedPolicies: actualVersionedPolicies
  , versionedValidators: actualVersionedValidators
  } <-
    getActualVersionedPoliciesAndValidators
      sidechainParams
      version

  { versionedPolicies: expectedVersionedPolicies
  , versionedValidators: expectedVersionedValidators
  } <-
    getExpectedVersionedPoliciesAndValidators
      sidechainParams
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

  validatorsTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned validators"
      )
      $ List.toUnfoldable versionedValidators

  logInfo'
    $ "Insert policies for version"
    <> show version

  policiesTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Insert versioned policies"
      )
      $ List.toUnfoldable versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

invalidateVersion ::
  forall r.
  SidechainParams ->
  Int ->
  Run (APP + r) (Array TransactionHash)
invalidateVersion sidechainParams version = do
  { versionedPolicies, versionedValidators } <-
    getExpectedVersionedPoliciesAndValidators
      sidechainParams
      version

  validatorsTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned validators"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst versionedValidators
  policiesTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Invalidate versioned policies"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst versionedPolicies

  pure (validatorsTxIds <> policiesTxIds)

updateVersion ::
  forall r.
  SidechainParams ->
  Int -> -- old version
  Int -> -- new version
  Run (APP + r) (Array TransactionHash)
updateVersion sidechainParams oldVersion newVersion = do
  { versionedPolicies: oldVersionedPolicies
  , versionedValidators: oldVersionedValidators
  } <- getExpectedVersionedPoliciesAndValidators sidechainParams
    oldVersion

  { versionedPolicies: newVersionedPolicies
  , versionedValidators: newVersionedValidators
  } <- getExpectedVersionedPoliciesAndValidators sidechainParams
    newVersion

  newValidatorsTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned validators"
      )
      $ List.toUnfoldable newVersionedValidators
  newPoliciesTxIds <-
    traverse
      ( Utils.insertVersionLookupsAndConstraints sidechainParams newVersion >=>
          Utils.Transaction.balanceSignAndSubmit "Update new versioned policies"
      )
      $ List.toUnfoldable newVersionedPolicies

  oldValidatorsTxIds <-
    traverse
      ( Utils.invalidateVersionLookupsAndConstraints sidechainParams oldVersion
          >=>
            Utils.Transaction.balanceSignAndSubmit
              "Update old versioned validators"
      )
      $ Array.fromFoldable
      $ List.nub
      $ map fst oldVersionedValidators
  oldPoliciesTxIds <-
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
getExpectedVersionedPoliciesAndValidators ::
  forall r.
  SidechainParams ->
  Int ->
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getExpectedVersionedPoliciesAndValidators sidechainParams version =
  case version of
    1 -> V1.getVersionedPoliciesAndValidators sidechainParams
    2 -> V2.getVersionedPoliciesAndValidators sidechainParams
    _ -> throw $ GenericInternalError ("Invalid version: " <> show version)

getCommitteeSelectionPoliciesAndValidators ::
  forall r.
  SidechainParams ->
  Int ->
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getCommitteeSelectionPoliciesAndValidators sidechainParams version = do
  case version of
    1 -> V1.getCommitteeSelectionPoliciesAndValidators sidechainParams
    2 -> V2.getCommitteeSelectionPoliciesAndValidators sidechainParams
    _ -> throw $ GenericInternalError ("Invalid version: " <> show version)

getNativeTokenManagementPoliciesAndValidators ::
  forall r.
  SidechainParams ->
  Int ->
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getNativeTokenManagementPoliciesAndValidators sidechainParams version = do
  case version of
    1 -> V1.getNativeTokenManagementPoliciesAndValidators sidechainParams
    2 -> V2.getNativeTokenManagementPoliciesAndValidators sidechainParams
    _ -> throw $ GenericInternalError ("Invalid version: " <> show version)

-- | Get the list of "actual" validators and minting policies that should be versioned.
--
-- See Note [Expected vs actual versioned policies and validators]
--
-- Used in the 'ListVersionedScripts' endpoint.
getActualVersionedPoliciesAndValidators ::
  forall r.
  SidechainParams ->
  Int ->
  Run (READER Env + EXCEPT OffchainError + TRANSACTION + WALLET + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }

getActualVersionedPoliciesAndValidators sidechainParams version =
  do
    vValidator <- versionOracleValidator sidechainParams

    -- Get UTxOs located at the version oracle validator script address
    versionOracleValidatorAddr <- toAddress (PlutusScript.hash vValidator)
    scriptUtxos <- Effect.utxosAt versionOracleValidatorAddr

    -- Get scripts that should be versioned
    { versionedPolicies, versionedValidators } <-
      getExpectedVersionedPoliciesAndValidators sidechainParams
        version

    -- Create Map of type 'Map ScriptHash (ScriptId, Script)' for fast retrieval
    -- of versioned scripts based on 'ScriptHash'.
    let
      versionedPoliciesIndexedByHash =
        Map.fromFoldable
          $ map (\t@(Tuple _ script) -> PlutusScript.hash script /\ t)
          $ versionedPolicies

      versionedValidatorsIndexedByHash =
        Map.fromFoldable
          $ map (\t@(Tuple _ script) -> PlutusScript.hash script /\ t)
          $ versionedValidators

    -- Get script hashes of versioned scripts that are linked to the version
    -- oracle validator.
    let
      (actualVersionedScriptHashes :: List ScriptHash) =
        List.catMaybes
          $ map
              ( \(TransactionOutput { scriptRef, datum: outputDatum }) ->
                  case (scriptRef /\ outputDatum) of
                    ( Just (PlutusScriptRef plutusScript) /\ Just
                        (OutputDatum datum)
                    ) -> do
                      Types.VersionOracleDatum
                        { versionOracle: Types.VersionOracle { version: v } } <-
                        fromData datum
                      if v == BigNum.fromInt version then pure $ PlutusScript.hash
                        plutusScript
                      else Nothing
                    _ -> Nothing
              )
          $ Map.values scriptUtxos

    -- Compute 'Map ScriptId Script' based on the 'ScriptHash' of actual
    -- versioned scripts.
    let
      actualVersionedPolicies =
        List.catMaybes $
          map
            (\scriptHash -> Map.lookup scriptHash versionedPoliciesIndexedByHash)
            actualVersionedScriptHashes

      actualVersionedValidators =
        List.catMaybes $
          map
            (\scriptHash -> Map.lookup scriptHash versionedValidatorsIndexedByHash)
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
