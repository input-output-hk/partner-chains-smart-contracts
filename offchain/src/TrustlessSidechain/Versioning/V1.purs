module TrustlessSidechain.Versioning.V1
  ( getVersionedPolicies
  , getVersionedValidators
  , getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator)
import Data.Map as Map
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
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Types as Types

-- | Validators to store in the versioning system.
getVersionedValidators ∷
  SidechainParams →
  Contract (Map.Map Types.ScriptId Validator)
getVersionedValidators sp = do
  -- Getting validators and policies to version
  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator sp
  { dParameterValidator } ← DParameter.getDParameterValidatorAndAddress sp
  -----------------------------------
  pure $ Map.fromFoldable
    [ Types.MerkleRootTokenValidator /\ merkleRootTokenValidator
    , Types.DParameterValidator /\ dParameterValidator
    ]

-- | Minting policies to store in the versioning system.
getVersionedPolicies ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Contract (Map.Map Types.ScriptId MintingPolicy)
getVersionedPolicies { sidechainParams: sp, atmsKind } = do
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

  ds ← DistributedSet.getDs (unwrap sp).genesisUtxo
  { dsKeyPolicy } ← DistributedSet.getDsKeyPolicy ds
  committeeHashPolicy ← CommitteeOraclePolicy.committeeOraclePolicy $
    CommitteeOraclePolicy.InitCommitteeHashMint
      { icTxOutRef: (unwrap sp).genesisUtxo }
  pure $ Map.fromFoldable
    [ Types.MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy
    , Types.FUELMintingPolicy /\ fuelMintingPolicy
    , Types.FUELBurningPolicy /\ fuelBurningPolicy
    , Types.DSKeyPolicy /\ dsKeyPolicy
    , Types.CommitteeHashPolicy /\ committeeHashPolicy
    , Types.CommitteeCertificateVerificationPolicy /\
        committeeCertificateVerificationMintingPolicy
    , Types.CommitteeOraclePolicy /\ committeeOraclePolicy
    , Types.DParameterPolicy /\ dParameterMintingPolicy
    ]

getVersionedPoliciesAndValidators ∷
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  } →
  Contract
    { versionedPolicies ∷ Map.Map Types.ScriptId MintingPolicy
    , versionedValidators ∷ Map.Map Types.ScriptId Validator
    }
getVersionedPoliciesAndValidators params = do
  versionedPolicies ← getVersionedPolicies params
  versionedValidators ← getVersionedValidators params.sidechainParams
  pure $ { versionedPolicies, versionedValidators }
