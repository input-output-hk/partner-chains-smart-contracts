module TrustlessSidechain.Versioning.V1
  ( getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Value as Value
import Data.Map as Map
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.Checkpoint.Types
  ( CheckpointParameter(CheckpointParameter)
  )
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds
  , CommitteeCertificateMint(CommitteeCertificateMint)
  )
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeOraclePolicy as CommitteeOraclePolicy
import TrustlessSidechain.DParameter.Utils as DParameter
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELBurningPolicy.V1 as FUELBurningPolicy.V1
import TrustlessSidechain.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.PermissionedCandidates.Utils as PermissionedCandidates
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (assetClass)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.Utils.Logging
  ( InternalError(InvalidScript)
  , OffchainError(InternalError)
  )
import TrustlessSidechain.Versioning.Types as Types

getVersionedPoliciesAndValidators ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Contract
    { versionedPolicies ∷ Map.Map Types.ScriptId MintingPolicy
    , versionedValidators ∷ Map.Map Types.ScriptId Validator
    }
getVersionedPoliciesAndValidators { sidechainParams: sp, atmsKind } = do
  -- Getting policies to version
  -----------------------------------
  -- some awkwardness that we need the committee hash policy first.
  { committeeOraclePolicy } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sp

  let
    committeeCertificateMint =
      CommitteeCertificateMint
        { thresholdNumerator: (unwrap sp).thresholdNumerator
        , thresholdDenominator: (unwrap sp).thresholdDenominator
        }

  { committeeCertificateVerificationMintingPolicy } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
      { committeeCertificateMint, sidechainParams: sp }
      atmsKind

  { merkleRootTokenMintingPolicy } ← MerkleRoot.getMerkleRootTokenMintingPolicy
    sp
  { fuelMintingPolicy } ← FUELMintingPolicy.V1.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V1.getFuelBurningPolicy sp
  { dParameterMintingPolicy } ←
    DParameter.getDParameterMintingPolicyAndCurrencySymbol sp
  { permissionedCandidatesMintingPolicy } ←
    PermissionedCandidates.getPermissionedCandidatesMintingPolicyAndCurrencySymbol
      sp

  ds ← DistributedSet.getDs (unwrap sp).genesisUtxo
  { dsKeyPolicy } ← DistributedSet.getDsKeyPolicy ds
  committeeHashPolicy ← CommitteeOraclePolicy.committeeOraclePolicy $
    CommitteeOraclePolicy.InitCommitteeHashMint
      { icTxOutRef: (unwrap sp).genesisUtxo }

  let
    versionedPolicies = Map.fromFoldable
      [ Types.MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy
      , Types.FUELMintingPolicy /\ fuelMintingPolicy
      , Types.FUELBurningPolicy /\ fuelBurningPolicy
      , Types.DSKeyPolicy /\ dsKeyPolicy
      , Types.CommitteeHashPolicy /\ committeeHashPolicy
      , Types.CommitteeCertificateVerificationPolicy /\
          committeeCertificateVerificationMintingPolicy
      , Types.CommitteeOraclePolicy /\ committeeOraclePolicy
      , Types.DParameterPolicy /\ dParameterMintingPolicy
      , Types.PermissionedCandidatesPolicy /\ permissionedCandidatesMintingPolicy
      ]

  -- Helper currency symbols
  -----------------------------------
  { committeeOracleCurrencySymbol } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sp

  { committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
      { committeeCertificateMint, sidechainParams: sp }
      atmsKind

  merkleRootTokenCurrencySymbol ← liftContractM
    (show (InternalError (InvalidScript "MerkleRootTokenMintingPolicy")))
    (Value.scriptCurrencySymbol merkleRootTokenMintingPolicy)

  { checkpointCurrencySymbol } ← Checkpoint.getCheckpointPolicy sp

  -- Getting validators to version
  -----------------------------------
  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator sp
  { validator: committeeHashValidator } ←
    do
      let
        uch = UpdateCommitteeHash
          { sidechainParams: sp
          , committeeOracleCurrencySymbol: committeeOracleCurrencySymbol
          , merkleRootTokenCurrencySymbol
          , committeeCertificateVerificationCurrencySymbol
          }
      getUpdateCommitteeHashValidator uch

  { dParameterValidator } ← DParameter.getDParameterValidatorAndAddress sp
  { permissionedCandidatesValidator } ←
    PermissionedCandidates.getPermissionedCandidatesValidatorAndAddress sp

  checkpointValidator ← do
    let
      checkpointParam = CheckpointParameter
        { sidechainParams: sp
        , checkpointAssetClass: assetClass checkpointCurrencySymbol
            Checkpoint.initCheckpointMintTn
        , committeeOracleCurrencySymbol
        , committeeCertificateVerificationCurrencySymbol
        }
    Checkpoint.checkpointValidator checkpointParam

  let
    versionedValidators = Map.fromFoldable
      [ Types.MerkleRootTokenValidator /\ merkleRootTokenValidator
      , Types.CheckpointValidator /\ checkpointValidator
      , Types.CommitteeHashValidator /\ committeeHashValidator
      , Types.DParameterValidator /\ dParameterValidator
      , Types.PermissionedCandidatesValidator /\ permissionedCandidatesValidator
      ]

  pure $ { versionedPolicies, versionedValidators }
