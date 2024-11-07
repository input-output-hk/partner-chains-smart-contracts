module TrustlessSidechain.Versioning.V2
  ( getCommitteeSelectionPoliciesAndValidators
  , getVersionedPoliciesAndValidators
  , getNativeTokenManagementPoliciesAndValidators
  ) where

import Contract.Prelude

import Cardano.Types.PlutusScript (PlutusScript)
import Contract.Transaction (TransactionInput)
import Data.List (List)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Versioning.Types as Types
import Type.Row (type (+))

-- | Validators and policies to store in the versioning system.
getVersionedPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + r)
    { versionedPolicies :: (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators :: (List (Tuple Types.ScriptId PlutusScript))
    }
getVersionedPoliciesAndValidators genesisUtxo = do
  committeeScripts <- getCommitteeSelectionPoliciesAndValidators genesisUtxo

  pure $ committeeScripts

getCommitteeSelectionPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + r)
    { versionedPolicies :: (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators :: (List (Tuple Types.ScriptId PlutusScript))
    }
getCommitteeSelectionPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }

getNativeTokenManagementPoliciesAndValidators ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + r)
    { versionedPolicies :: (List (Tuple Types.ScriptId PlutusScript))
    , versionedValidators :: (List (Tuple Types.ScriptId PlutusScript))
    }
getNativeTokenManagementPoliciesAndValidators _ =
  let
    versionedPolicies = List.fromFoldable []
    versionedValidators = List.fromFoldable []
  in
    pure $ { versionedPolicies, versionedValidators }
