module TrustlessSidechain.InitSidechain.Init
  ( getScriptsToInsert
  , insertScriptsIdempotent
  ) where

import Contract.Prelude

import Cardano.Types.PlutusScript (PlutusScript)
import Contract.Transaction (TransactionHash)
import Data.List (List, filter)
import Data.List as List
import Run (Run)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Versioning (getActualVersionedPoliciesAndValidators)
import TrustlessSidechain.Versioning.ScriptId as Types
import TrustlessSidechain.Versioning.Types (ScriptId)
import TrustlessSidechain.Versioning.Utils as Utils
import Type.Row (type (+))

insertScriptsIdempotent ::
  forall r.
  ( SidechainParams ->
    Run (APP + r)
      { versionedPolicies :: List (Tuple ScriptId PlutusScript)
      , versionedValidators :: List (Tuple ScriptId PlutusScript)
      }
  ) ->
  SidechainParams ->
  Run (APP + r)
    (Array TransactionHash)
insertScriptsIdempotent f sidechainParams = do
  scripts <- f sidechainParams

  toInsert ::
    { versionedPolicies :: List (Tuple ScriptId PlutusScript)
    , versionedValidators :: List (Tuple ScriptId PlutusScript)
    } <- getScriptsToInsert sidechainParams scripts

  validatorsTxIds <-
    ( traverse ::
        forall m a b. Applicative m => (a -> m b) -> Array a -> m (Array b)
    )
      ( Utils.insertVersionLookupsAndConstraints sidechainParams >=>
          Utils.Transaction.balanceSignAndSubmit
            "Initialize versioned validators"
      )
      $ List.toUnfoldable (toInsert.versionedValidators)
  policiesTxIds <-
    ( traverse ::
        forall m a b. Applicative m => (a -> m b) -> Array a -> m (Array b)
    )
      ( Utils.insertVersionLookupsAndConstraints sidechainParams >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned policies"
      )
      $ List.toUnfoldable (toInsert.versionedPolicies)

  pure $ policiesTxIds <> validatorsTxIds

getScriptsToInsert ::
  forall r.
  SidechainParams ->
  { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
  , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
  } ->
  Run (APP + r)
    { versionedPolicies :: List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators :: List (Tuple Types.ScriptId PlutusScript)
    }
getScriptsToInsert
  sidechainParams
  toFilterScripts = do

  comparisonScripts <-
    getActualVersionedPoliciesAndValidators
      sidechainParams

  let
    filterScripts :: forall a. Eq a => List a -> List a -> List a
    filterScripts sublist list = filter (not <<< flip elem list) sublist

  pure
    { versionedPolicies: filterScripts toFilterScripts.versionedPolicies
        comparisonScripts.versionedPolicies
    , versionedValidators: filterScripts toFilterScripts.versionedValidators
        comparisonScripts.versionedValidators
    }