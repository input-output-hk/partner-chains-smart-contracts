module TrustlessSidechain.Versioning.V1
  ( getCommitteeSelectionPoliciesAndValidators
  , getCheckpointPoliciesAndValidators
  , getVersionedPoliciesAndValidators
  , getFuelPoliciesAndValidators
  , getDsPoliciesAndValidators
  , getMerkleRootPoliciesAndValidators
  , getNativeTokenManagementPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Scripts (MintingPolicy, Validator)
import Data.List (List)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointParameter(CheckpointParameter)
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.FUELBurningPolicy.V1 as FUELBurningPolicy.V1
import TrustlessSidechain.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveAuthPolicy
  , reserveValidator
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.Versioning.Types (ScriptId(..))
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

getVersionedPoliciesAndValidators ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getVersionedPoliciesAndValidators { sidechainParams: sp, atmsKind } = do
  committeeScripts ← getCommitteeSelectionPoliciesAndValidators atmsKind sp
  checkpointScripts ← getCheckpointPoliciesAndValidators sp
  fuelScripts ← getFuelPoliciesAndValidators sp
  dsScripts ← getDsPoliciesAndValidators sp
  merkleRootScripts ← getMerkleRootPoliciesAndValidators sp
  nativeTokenManagementScripts ← getNativeTokenManagementPoliciesAndValidators sp

  pure $ committeeScripts
    <> checkpointScripts
    <> fuelScripts
    <> dsScripts
    <> merkleRootScripts
    <> nativeTokenManagementScripts

getMerkleRootPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getMerkleRootPoliciesAndValidators sp = do
  { mintingPolicy: merkleRootTokenMintingPolicy } ←
    MerkleRoot.merkleRootCurrencyInfo sp

  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator sp

  let
    versionedPolicies = List.fromFoldable
      [ MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy ]
    versionedValidators = List.fromFoldable
      [ MerkleRootTokenValidator /\ merkleRootTokenValidator ]

  pure { versionedPolicies, versionedValidators }

getCommitteeSelectionPoliciesAndValidators ∷
  ∀ r.
  ATMSKinds →
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getCommitteeSelectionPoliciesAndValidators atmsKind sp =
  do
    -- Getting policies to version
    -----------------------------------
    -- some awkwardness that we need the committee hash policy first.
    { mintingPolicy: committeeOraclePolicy
    } ←
      CommitteeOraclePolicy.committeeOracleCurrencyInfo sp

    let
      committeeCertificateMint =
        CommitteeCertificateMint
          { thresholdNumerator: (unwrap sp).thresholdNumerator
          , thresholdDenominator: (unwrap sp).thresholdDenominator
          }

    { mintingPolicy: committeeCertificateVerificationMintingPolicy } ←
      CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
        { committeeCertificateMint, sidechainParams: sp }
        atmsKind

    let
      versionedPolicies = List.fromFoldable
        [ CommitteeCertificateVerificationPolicy /\
            committeeCertificateVerificationMintingPolicy
        , CommitteeOraclePolicy /\ committeeOraclePolicy
        ]

    -- Getting validators to version
    -----------------------------------
    { validator: committeeHashValidator } ←
      do
        getUpdateCommitteeHashValidator sp
    committeeCandidateValidator ← getCommitteeCandidateValidator sp

    let
      versionedValidators = List.fromFoldable
        [ CommitteeHashValidator /\ committeeHashValidator
        , CommitteeCandidateValidator /\ committeeCandidateValidator
        ]

    pure $ { versionedPolicies, versionedValidators }

getCheckpointPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getCheckpointPoliciesAndValidators sp = do
  checkpointAssetClass ← Checkpoint.checkpointAssetClass sp

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  checkpointValidator ← do
    let
      checkpointParam = CheckpointParameter
        { sidechainParams: sp
        , checkpointAssetClass
        }
    Checkpoint.checkpointValidator checkpointParam versionOracleConfig

  let
    versionedValidators = List.fromFoldable
      [ CheckpointValidator /\ checkpointValidator
      ]

  pure $ { versionedPolicies: mempty, versionedValidators }

getNativeTokenManagementPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getNativeTokenManagementPoliciesAndValidators sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  reserveAuthPolicy' ← reserveAuthPolicy versionOracleConfig
  reserveValidator' ← reserveValidator versionOracleConfig

  let
    versionedPolicies = List.fromFoldable
      [ ReserveAuthPolicy /\ reserveAuthPolicy'
      ]
    versionedValidators = List.fromFoldable
      [ ReserveValidator /\ reserveValidator'
      ]

  pure $ { versionedPolicies, versionedValidators }

-- | Return policies and validators needed for FUEL minting
-- | and burning.
getFuelPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getFuelPoliciesAndValidators sp = do
  { fuelMintingPolicy } ← FUELMintingPolicy.V1.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V1.getFuelBurningPolicy sp

  let
    versionedPolicies = List.fromFoldable
      [ FUELMintingPolicy /\ fuelMintingPolicy
      , FUELBurningPolicy /\ fuelBurningPolicy
      ]
    versionedValidators = List.fromFoldable []

  pure { versionedPolicies, versionedValidators }

-- | Get V1 policies and validators for the
-- | Ds* types.
getDsPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    }
getDsPoliciesAndValidators sp = do
  ds ← DistributedSet.getDs sp
  { mintingPolicy: dsKeyPolicy } ← DistributedSet.getDsKeyPolicy ds

  let
    versionedPolicies = List.fromFoldable [ DsKeyPolicy /\ dsKeyPolicy ]
    versionedValidators = List.fromFoldable []

  pure { versionedPolicies, versionedValidators }
