module TrustlessSidechain.Versioning.V2
  ( getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Scripts (MintingPolicy, Validator)
import Data.List (List)
import Data.List as List
import TrustlessSidechain.FUELBurningPolicy.V2 as FUELBurningPolicy.V2
import TrustlessSidechain.FUELMintingPolicy.V2 as FUELMintingPolicy.V2
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Types as Types

-- | Validators to store in the versioning system.
getVersionedValidators ∷
  SidechainParams →
  Contract (List (Tuple Types.ScriptId Validator))
getVersionedValidators _sp = do
  -- Getting validators to version
  -----------------------------------
  pure $ List.fromFoldable []

-- | Minting policies to store in the versioning system.
getVersionedPolicies ∷
  SidechainParams →
  Contract (List (Tuple Types.ScriptId MintingPolicy))
getVersionedPolicies sp = do
  -- Getting policies to version
  -----------------------------------
  { fuelMintingPolicy } ← FUELMintingPolicy.V2.getFuelMintingPolicy sp
  { fuelBurningPolicy } ← FUELBurningPolicy.V2.getFuelBurningPolicy sp
  pure $ List.fromFoldable
    [ Types.FUELMintingPolicy /\ fuelMintingPolicy
    , Types.FUELBurningPolicy /\ fuelBurningPolicy
    ]

-- | Validators and policies to store in the versioning system.
getVersionedPoliciesAndValidators ∷
  SidechainParams →
  Contract
    { versionedPolicies ∷ (List (Tuple Types.ScriptId MintingPolicy))
    , versionedValidators ∷ (List (Tuple Types.ScriptId Validator))
    }
getVersionedPoliciesAndValidators sp = do
  versionedPolicies ← getVersionedPolicies sp
  versionedValidators ← getVersionedValidators sp
  pure $ { versionedPolicies, versionedValidators }
