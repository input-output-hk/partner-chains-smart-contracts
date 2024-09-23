module TrustlessSidechain.Versioning.V1
  ( getCommitteeSelectionPoliciesAndValidators
  , getVersionedPoliciesAndValidators
  , getNativeTokenManagementPoliciesAndValidators
  , getGovernancePoliciesAndValidators
  ) where

import Contract.Prelude

import Cardano.Types.BigInt as BigInt
import Cardano.Types.PlutusScript (PlutusScript)
import Data.List (List)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.Effects.Env (Env, READER, ask)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Governance (Governance(MultiSig))
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  )
import TrustlessSidechain.Governance.MultiSig
  ( multisigGovPolicy
  )
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( illiquidCirculationSupplyValidator
  )
import TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveAuthPolicy
  , reserveValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
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

getVersionedPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple ScriptId PlutusScript)
    }
getVersionedPoliciesAndValidators sidechainParams = do
  committeeScripts ← getCommitteeSelectionPoliciesAndValidators sidechainParams
  nativeTokenManagementScripts ← getNativeTokenManagementPoliciesAndValidators
    sidechainParams
  governance ← getGovernancePoliciesAndValidators sidechainParams

  pure $ committeeScripts
    <> nativeTokenManagementScripts
    <> governance

getGovernancePoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple ScriptId PlutusScript)
    }
getGovernancePoliciesAndValidators sp = do
  governanceScript ← multisigGovPolicy $ MultiSigGovParams
    { governanceMembers: [ unwrap (unwrap (unwrap sp).governanceAuthority) ]
    , requiredSignatures: BigInt.fromInt 1
    }
  pure $
    { versionedPolicies: List.fromFoldable
        [ GovernancePolicy /\ governanceScript ]
    , versionedValidators: mempty
    }

getCommitteeSelectionPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple ScriptId PlutusScript)
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
    committeeCandidateValidator ← getCommitteeCandidateValidator sp

    let
      versionedValidators = List.fromFoldable
        [ CommitteeCandidateValidator /\ committeeCandidateValidator
        ]

    pure $ { versionedPolicies, versionedValidators }

getNativeTokenManagementPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (READER Env + EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple ScriptId PlutusScript)
    }
getNativeTokenManagementPoliciesAndValidators sp = do
  governance ← (_.governance) <$> ask
  case governance of
    -- The native token management system can only be used if the user specified
    -- parameters for the governance (currently only multisignature governance)
    Just (MultiSig msgp) → do
      versionOracleConfig ← Versioning.getVersionOracleConfig sp
      reserveAuthPolicy' ← reserveAuthPolicy versionOracleConfig
      reserveValidator' ← reserveValidator versionOracleConfig
      illiquidCirculationSupplyValidator' ←
        illiquidCirculationSupplyValidator versionOracleConfig
      governancePolicy ← multisigGovPolicy msgp

      let
        versionedPolicies = List.fromFoldable
          [ ReserveAuthPolicy /\ reserveAuthPolicy'
          , GovernancePolicy /\ governancePolicy
          ]
        versionedValidators = List.fromFoldable
          [ ReserveValidator /\ reserveValidator'
          , IlliquidCirculationSupplyValidator /\
              illiquidCirculationSupplyValidator'
          ]

      pure $ { versionedPolicies, versionedValidators }
    _ → pure { versionedPolicies: mempty, versionedValidators: mempty }
