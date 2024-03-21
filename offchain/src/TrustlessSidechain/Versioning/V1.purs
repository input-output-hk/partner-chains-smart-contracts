module TrustlessSidechain.Versioning.V1
  ( getVersionedPoliciesAndValidators
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
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( MerkleRootTokenValidator
      , CheckpointValidator
      , CommitteeHashValidator
      , CommitteeCandidateValidator
      , MerkleRootTokenPolicy
      , FUELMintingPolicy
      , FUELBurningPolicy
      , DsKeyPolicy
      , CommitteeCertificateVerificationPolicy
      , CommitteeOraclePolicy
      )
  )
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

  { mintingPolicy: merkleRootTokenMintingPolicy } ←
    MerkleRoot.merkleRootCurrencyInfo sp
  { fuelMintingPolicy } ← FUELMintingPolicy.V1.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V1.getFuelBurningPolicy sp

  ds ← DistributedSet.getDs sp
  { mintingPolicy: dsKeyPolicy } ← DistributedSet.getDsKeyPolicy ds

  let
    versionedPolicies = List.fromFoldable
      [ MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy
      , FUELMintingPolicy /\ fuelMintingPolicy
      , FUELBurningPolicy /\ fuelBurningPolicy
      , DsKeyPolicy /\ dsKeyPolicy
      , CommitteeCertificateVerificationPolicy /\
          committeeCertificateVerificationMintingPolicy
      , CommitteeOraclePolicy /\ committeeOraclePolicy
      ]

  checkpointAssetClass ← Checkpoint.checkpointAssetClass sp

  -- Getting validators to version
  -----------------------------------
  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator sp
  { validator: committeeHashValidator } ←
    do
      getUpdateCommitteeHashValidator sp

  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  checkpointValidator ← do
    let
      checkpointParam = CheckpointParameter
        { sidechainParams: sp
        , checkpointAssetClass
        }
    Checkpoint.checkpointValidator checkpointParam versionOracleConfig

  committeeCandidateValidator ← getCommitteeCandidateValidator sp

  let
    versionedValidators = List.fromFoldable
      [ MerkleRootTokenValidator /\ merkleRootTokenValidator
      , CheckpointValidator /\ checkpointValidator
      , CommitteeHashValidator /\ committeeHashValidator
      , CommitteeCandidateValidator /\ committeeCandidateValidator
      --, DParameterValidator /\ dParameterValidator
      --, PermissionedCandidatesValidator /\ permissionedCandidatesValidator
      ]

  pure $ { versionedPolicies, versionedValidators }
