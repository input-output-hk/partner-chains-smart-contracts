module TrustlessSidechain.Versioning.V2
  ( getVersionedPolicies
  , getVersionedValidators
  , getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator)
import Data.Map as Map
import TrustlessSidechain.FUELBurningPolicy.V2 as FUELBurningPolicy.V2
import TrustlessSidechain.FUELMintingPolicy.V2 as FUELMintingPolicy.V2
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Types as Types

-- | Validators to store in the versioning system.
getVersionedValidators ∷
  SidechainParams →
  Contract (Map.Map Types.ScriptId Validator)
getVersionedValidators _sp = do
  -- Getting validators and policies to version
  -----------------------------------
  pure $ Map.fromFoldable []

-- | Minting policies to store in the versioning system.
getVersionedPolicies ∷
  SidechainParams →
  Contract (Map.Map Types.ScriptId MintingPolicy)
getVersionedPolicies sp = do
  -- Getting policies to version
  -----------------------------------
  { fuelMintingPolicy } ← FUELMintingPolicy.V2.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V2.getFuelBurningPolicy sp
  pure $ Map.fromFoldable
    [ Types.FUELMintingPolicy /\ fuelMintingPolicy
    , Types.FUELBurningPolicy /\ fuelBurningPolicy
    ]

getVersionedPoliciesAndValidators ∷
  SidechainParams →
  Contract
    { versionedPolicies ∷ (Map.Map Types.ScriptId MintingPolicy)
    , versionedValidators ∷ (Map.Map Types.ScriptId Validator)
    }
getVersionedPoliciesAndValidators sp = do
  versionedPolicies ← getVersionedPolicies sp
  versionedValidators ← getVersionedValidators sp
  pure $ { versionedPolicies, versionedValidators }
