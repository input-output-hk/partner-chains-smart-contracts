module TrustlessSidechain.Versioning.V2
  ( getCommitteeSelectionPoliciesAndValidators
  , getCheckpointPoliciesAndValidators
  , getVersionedPoliciesAndValidators
  , getDsPoliciesAndValidators
  , getMerkleRootPoliciesAndValidators
  , getNativeTokenManagementPoliciesAndValidators
  ) where

import Contract.Prelude

import Cardano.Types.PlutusScript (PlutusScript)
import Data.List (List)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Types as Types
import Type.Row (type (+))

-- | Validators and policies to store in the versioning system.
getVersionedPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators ∷ (List (Tuple Types.ScriptId PlutusScript))
    }
getVersionedPoliciesAndValidators sp = do
  committeeScripts ← getCommitteeSelectionPoliciesAndValidators sp
  checkpointScripts ← getCheckpointPoliciesAndValidators sp
  dsScripts ← getDsPoliciesAndValidators sp
  merkleRootScripts ← getMerkleRootPoliciesAndValidators sp

  pure $ committeeScripts <> checkpointScripts <> dsScripts
    <> merkleRootScripts

getCommitteeSelectionPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators ∷ (List (Tuple Types.ScriptId PlutusScript))
    }
getCommitteeSelectionPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }

-- | Get V2 policies and validators for the Ds* types.
getDsPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getDsPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure { versionedPolicies, versionedValidators }

getCheckpointPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators ∷ (List (Tuple Types.ScriptId PlutusScript))
    }
getCheckpointPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }

getNativeTokenManagementPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators ∷ (List (Tuple Types.ScriptId PlutusScript))
    }
getNativeTokenManagementPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }

-- | Get V2 policies and validators for the
-- | Merkle Root.
getMerkleRootPoliciesAndValidators ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
    }
getMerkleRootPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure { versionedPolicies, versionedValidators }
