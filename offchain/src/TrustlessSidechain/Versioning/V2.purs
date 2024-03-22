module TrustlessSidechain.Versioning.V2
  ( getCommitteeSelectionPoliciesAndValidators
  , getCheckpointPoliciesAndValidators
  , getVersionedPoliciesAndValidators
  ) where

import Contract.Prelude

import Contract.Scripts (MintingPolicy, Validator)
import Data.List (List)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.FUELBurningPolicy.V2 as FUELBurningPolicy.V2
import TrustlessSidechain.FUELMintingPolicy.V2 as FUELMintingPolicy.V2
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Types as Types
import Type.Row (type (+))

-- | Validators to store in the versioning system.
getVersionedValidators
  ∷ SidechainParams
  → List (Tuple Types.ScriptId Validator)
getVersionedValidators _sp = do
  -- Getting validators to version
  -----------------------------------
  List.fromFoldable []

-- | Minting policies to store in the versioning system.
getVersionedPolicies
  ∷ ∀ r
  . SidechainParams
  → Run (EXCEPT OffchainError + r) (List (Tuple Types.ScriptId MintingPolicy))
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
getVersionedPoliciesAndValidators
  ∷ ∀ r
  . SidechainParams
  → Run (EXCEPT OffchainError + r)
      { versionedPolicies ∷ (List (Tuple Types.ScriptId MintingPolicy))
      , versionedValidators ∷ (List (Tuple Types.ScriptId Validator))
      }
getVersionedPoliciesAndValidators sp = do
  versionedPolicies ← getVersionedPolicies sp
  let versionedValidators = getVersionedValidators sp
  pure $ { versionedPolicies, versionedValidators }

getCommitteeSelectionPoliciesAndValidators
  ∷ ∀ r
  . SidechainParams
  → Run (EXCEPT OffchainError + r)
      { versionedPolicies ∷ (List (Tuple Types.ScriptId MintingPolicy))
      , versionedValidators ∷ (List (Tuple Types.ScriptId Validator))
      }
getCommitteeSelectionPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }

getCheckpointPoliciesAndValidators
  ∷ ∀ r
  . SidechainParams
  → Run (EXCEPT OffchainError + r)
      { versionedPolicies ∷ (List (Tuple Types.ScriptId MintingPolicy))
      , versionedValidators ∷ (List (Tuple Types.ScriptId Validator))
      }
getCheckpointPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }
