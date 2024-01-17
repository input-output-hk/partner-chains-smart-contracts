module TrustlessSidechain.Versioning.V1
  ( getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator)
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
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELBurningPolicy.V1 as FUELBurningPolicy.V1
import TrustlessSidechain.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeHash(UpdateCommitteeHash)
  )
import TrustlessSidechain.UpdateCommitteeHash.Utils
  ( getUpdateCommitteeHashValidator
  )
import TrustlessSidechain.Utils.Address (getCurrencySymbol)
import TrustlessSidechain.Versioning.Types
  ( ScriptId
      ( MerkleRootTokenValidator
      , CheckpointValidator
      , CommitteeHashValidator
      , MerkleRootTokenPolicy
      , FUELMintingPolicy
      , FUELBurningPolicy
      , DsKeyPolicy
      , CommitteeCertificateVerificationPolicy
      , CommitteeOraclePolicy
      )
  )

getVersionedPoliciesAndValidators ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Contract
    { versionedPolicies ∷ Map.Map ScriptId MintingPolicy
    , versionedValidators ∷ Map.Map ScriptId Validator
    }
getVersionedPoliciesAndValidators { sidechainParams: sp, atmsKind } = do
  -- Getting policies to version
  -----------------------------------
  -- some awkwardness that we need the committee hash policy first.
  { mintingPolicy: committeeOraclePolicy
  , currencySymbol: committeeOracleCurrencySymbol
  } ←
    CommitteeOraclePolicy.getCommitteeOraclePolicy sp

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

  { merkleRootTokenMintingPolicy } ← MerkleRoot.getMerkleRootTokenMintingPolicy
    sp
  { fuelMintingPolicy } ← FUELMintingPolicy.V1.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V1.getFuelBurningPolicy sp

  ds ← DistributedSet.getDs (unwrap sp).genesisUtxo
  { dsKeyPolicy } ← DistributedSet.getDsKeyPolicy ds

  let
    versionedPolicies = Map.fromFoldable
      [ MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy
      , FUELMintingPolicy /\ fuelMintingPolicy
      , FUELBurningPolicy /\ fuelBurningPolicy
      , DsKeyPolicy /\ dsKeyPolicy
      , CommitteeCertificateVerificationPolicy /\
          committeeCertificateVerificationMintingPolicy
      , CommitteeOraclePolicy /\ committeeOraclePolicy
      ]

  -- Helper currency symbols
  -----------------------------------
  { currencySymbol: committeeCertificateVerificationCurrencySymbol } ←
    CommitteeATMSSchemes.atmsCommitteeCertificateVerificationMintingPolicyFromATMSKind
      { committeeCertificateMint, sidechainParams: sp }
      atmsKind

  merkleRootTokenCurrencySymbol ←
    getCurrencySymbol MerkleRootTokenPolicy merkleRootTokenMintingPolicy

  checkpointAssetClass ← Checkpoint.getCheckpointAssetClass sp

  -- Getting validators to version
  -----------------------------------
  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator sp
  { validator: committeeHashValidator } ←
    do
      let
        uch = UpdateCommitteeHash
          { sidechainParams: sp
          , committeeOracleCurrencySymbol
          , merkleRootTokenCurrencySymbol
          , committeeCertificateVerificationCurrencySymbol
          }
      getUpdateCommitteeHashValidator uch

  checkpointValidator ← do
    let
      checkpointParam = CheckpointParameter
        { sidechainParams: sp
        , checkpointAssetClass
        , committeeOracleCurrencySymbol
        , committeeCertificateVerificationCurrencySymbol
        }
    Checkpoint.checkpointValidator checkpointParam

  let
    versionedValidators = Map.fromFoldable
      [ MerkleRootTokenValidator /\ merkleRootTokenValidator
      , CheckpointValidator /\ checkpointValidator
      , CommitteeHashValidator /\ committeeHashValidator
      --, DParameterValidator /\ dParameterValidator
      --, PermissionedCandidatesValidator /\ permissionedCandidatesValidator
      ]

  pure $ { versionedPolicies, versionedValidators }
