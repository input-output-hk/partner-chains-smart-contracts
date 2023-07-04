module TrustlessSidechain.Versioning.V1
  ( getVersionedPolicies
  , getVersionedValidators
  , getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator)
import Data.Map as Map
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELBurningPolicy.V1 as FUELBurningPolicy.V1
import TrustlessSidechain.FUELMintingPolicy.V1 as FUELMintingPolicy.V1
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Versioning.Types as Types

-- | Validators to store in the versioning system.
getVersionedValidators ∷
  SidechainParams →
  Contract (Map.Map Types.ScriptId Validator)
getVersionedValidators sp = do
  -- Getting validators and policies to version
  merkleRootTokenValidator ← MerkleRoot.merkleRootTokenValidator sp
  -----------------------------------
  pure $ Map.fromFoldable
    [ Types.MerkleRootTokenValidator /\ merkleRootTokenValidator
    ]

-- | Minting policies to store in the versioning system.
getVersionedPolicies ∷
  SidechainParams →
  Contract (Map.Map Types.ScriptId MintingPolicy)
getVersionedPolicies sp = do
  -- Getting policies to version
  -----------------------------------
  { merkleRootTokenMintingPolicy } ← MerkleRoot.getMerkleRootTokenMintingPolicy
    sp
  { fuelMintingPolicy } ← FUELMintingPolicy.V1.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V1.getFuelBurningPolicy sp

  ds ← DistributedSet.getDs (unwrap sp).genesisUtxo
  { dsKeyPolicy } ← DistributedSet.getDsKeyPolicy ds
  { committeeHashPolicy } ← UpdateCommitteeHash.getCommitteeHashPolicy sp
  pure $ Map.fromFoldable
    [ Types.MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy
    , Types.FUELMintingPolicy /\ fuelMintingPolicy
    , Types.FUELBurningPolicy /\ fuelBurningPolicy
    , Types.DSKeyPolicy /\ dsKeyPolicy
    , Types.CommitteeHashPolicy /\ committeeHashPolicy
    ]

getVersionedPoliciesAndValidators ∷
  SidechainParams →
  Contract
    { versionedPolicies ∷ Map.Map Types.ScriptId MintingPolicy
    , versionedValidators ∷ Map.Map Types.ScriptId Validator
    }
getVersionedPoliciesAndValidators sp = do
  versionedPolicies ← getVersionedPolicies sp
  versionedValidators ← getVersionedValidators sp
  pure $ { versionedPolicies, versionedValidators }
