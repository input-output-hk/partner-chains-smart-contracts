module TrustlessSidechain.Versioning.V1
  ( getCommitteeSelectionPoliciesAndValidators
  , getVersionedPoliciesAndValidators
  , getNativeTokenManagementPoliciesAndValidators
  , getVersionedPoliciesAndValidatorsScriptIds
  ) where

import Contract.Prelude

import Cardano.Types.PlutusScript (PlutusScript)
import Contract.Transaction (TransactionInput)
import Data.List (List)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( illiquidCirculationSupplyValidator
  )
import TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveAuthPolicy
  , reserveValidator
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( GovernancePolicy
      , IlliquidCirculationSupplyValidator
      , ReserveAuthPolicy
      , ReserveValidator
      , CommitteeCandidateValidator
      )
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

getVersionedPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple ScriptId PlutusScript)
    , versionedValidators :: List (Tuple ScriptId PlutusScript)
    }
getVersionedPoliciesAndValidators genesisUtxo = do
  committeeScripts <- getCommitteeSelectionPoliciesAndValidators genesisUtxo
  nativeTokenManagementScripts <- getNativeTokenManagementPoliciesAndValidators
    genesisUtxo

  pure $ committeeScripts
    <> nativeTokenManagementScripts

getVersionedPoliciesAndValidatorsScriptIds ::
  { versionedPolicies :: List ScriptId
  , versionedValidators :: List ScriptId
  }
getVersionedPoliciesAndValidatorsScriptIds = do
  let
    versionedPolicies = List.fromFoldable
      [ ReserveAuthPolicy
      , GovernancePolicy
      ]
    versionedValidators = List.fromFoldable
      [ ReserveValidator
      , IlliquidCirculationSupplyValidator
      , CommitteeCandidateValidator
      ]
  { versionedPolicies, versionedValidators }

getCommitteeSelectionPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple ScriptId PlutusScript)
    , versionedValidators :: List (Tuple ScriptId PlutusScript)
    }
getCommitteeSelectionPoliciesAndValidators sp =
  do
    -- Getting policies to version
    -----------------------------------
    -- some awkwardness that we need the committee hash policy first.

    let
      versionedPolicies = List.fromFoldable
        [
        ]

    -- Getting validators to version
    -----------------------------------
    committeeCandidateValidator <- getCommitteeCandidateValidator sp

    let
      versionedValidators = List.fromFoldable
        [ CommitteeCandidateValidator /\ committeeCandidateValidator
        ]

    pure $ { versionedPolicies, versionedValidators }

getNativeTokenManagementPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies :: List (Tuple ScriptId PlutusScript)
    , versionedValidators :: List (Tuple ScriptId PlutusScript)
    }
getNativeTokenManagementPoliciesAndValidators sp = do
  versionOracleConfig <- Versioning.getVersionOracleConfig sp
  reserveAuthPolicy' <- reserveAuthPolicy versionOracleConfig
  reserveValidator' <- reserveValidator versionOracleConfig
  illiquidCirculationSupplyValidator' <-
    illiquidCirculationSupplyValidator versionOracleConfig

  let
    versionedPolicies = List.fromFoldable
      [ ReserveAuthPolicy /\ reserveAuthPolicy'
      ]
    versionedValidators = List.fromFoldable
      [ ReserveValidator /\ reserveValidator'
      , IlliquidCirculationSupplyValidator /\
          illiquidCirculationSupplyValidator'
      ]

  pure $ { versionedPolicies, versionedValidators }
