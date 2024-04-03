module TrustlessSidechain.InitSidechain.Init
  ( getInitTokenStatus
  , getScriptsToInsert
  , init
  , initTokenStatus
  , insertScriptsIdempotent
  ) where

import Contract.Prelude

import Contract.AssocMap as Plutus.Map
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction (TransactionHash)
import Contract.Value (CurrencySymbol, TokenName, Value)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.List (List, filter)
import Data.List as List
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.InitSidechain.Utils (initTokenCurrencyInfo)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import TrustlessSidechain.Utils.Utxos (getOwnUTxOsTotalValue)
import TrustlessSidechain.Versioning (getActualVersionedPoliciesAndValidators)
import TrustlessSidechain.Versioning.ScriptId as Types
import TrustlessSidechain.Versioning.Types (ScriptId)
import TrustlessSidechain.Versioning.Utils as Utils
import Type.Row (type (+))

insertScriptsIdempotent ∷
  ∀ r.
  ( SidechainParams →
    Int →
    Run (APP + r)
      { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
      , versionedValidators ∷ List (Tuple ScriptId Validator)
      }
  ) →
  SidechainParams →
  ATMSKinds →
  Int →
  Run (APP + r)
    (Array TransactionHash)
insertScriptsIdempotent f sidechainParams initATMSKind version = do
  scripts ← f sidechainParams version

  toInsert ∷
    { versionedPolicies ∷ List (Tuple ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple ScriptId Validator)
    } ← getScriptsToInsert sidechainParams initATMSKind scripts version

  validatorsTxIds ←
    (traverse ∷ ∀ m a b. Applicative m ⇒ (a → m b) → Array a → m (Array b))
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit
            "Initialize versioned validators"
      )
      $ List.toUnfoldable (toInsert.versionedValidators)
  policiesTxIds ←
    (traverse ∷ ∀ m a b. Applicative m ⇒ (a → m b) → Array a → m (Array b))
      ( Utils.initializeVersionLookupsAndConstraints sidechainParams version >=>
          Utils.Transaction.balanceSignAndSubmit "Initialize versioned policies"
      )
      $ List.toUnfoldable (toInsert.versionedPolicies)

  pure $ policiesTxIds <> validatorsTxIds

getScriptsToInsert ∷
  ∀ r.
  SidechainParams →
  ATMSKinds →
  { versionedPolicies ∷ List (Tuple Types.ScriptId MintingPolicy)
  , versionedValidators ∷ List (Tuple Types.ScriptId Validator)
  } →
  Int →
  Run (APP + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId MintingPolicy)
    , versionedValidators ∷ List (Tuple Types.ScriptId Validator)
    }
getScriptsToInsert
  sidechainParams
  initATMSKind
  toFilterScripts
  version = do

  comparisonScripts ←
    getActualVersionedPoliciesAndValidators
      { atmsKind: initATMSKind, sidechainParams }
      version

  let
    filterScripts ∷ ∀ a. Eq a ⇒ List a → List a → List a
    filterScripts sublist list = filter (not <<< flip elem list) sublist

  pure
    { versionedPolicies: filterScripts toFilterScripts.versionedPolicies
        comparisonScripts.versionedPolicies
    , versionedValidators: filterScripts toFilterScripts.versionedValidators
        comparisonScripts.versionedValidators
    }

-- | Perform a token initialization action, if the corresponding
-- | init token exists. If it doesn't, throw an `InvalidInitState`
-- | error.
init ∷
  ∀ r.
  (String → SidechainParams → Run (APP + r) TransactionHash) →
  String →
  TokenName →
  SidechainParams →
  Run (APP + r) TransactionHash
init f op nm sp = do
  tokenExists ← map (Plutus.Map.member nm <<< _.initTokenStatusData)
    (getInitTokenStatus sp)

  unless tokenExists
    ( throw
        $ InvalidInitState
        $ "Init token does not exist when attempting to run "
        <> op
    )

  f op sp

-- | Get the init token data for the own wallet. Used in InitTokenStatus
-- | endpoint.
getInitTokenStatus ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { initTokenStatusData ∷ Plutus.Map.Map TokenName BigInt }
getInitTokenStatus scParams = do
  { currencySymbol } ← initTokenCurrencyInfo scParams

  -- NOTE: If Value later exposes a way to filter by currency (or to `map` or
  -- `lookup`), save a little computation by doing that before combining in
  -- getOwnUTxOsTotalValue.
  map (initTokenStatus currencySymbol) getOwnUTxOsTotalValue

-- | Get the init token data for the given `CurrencySymbol` from a given
-- | `Value`. Used in the InitTokenStatus endpoint.
initTokenStatus ∷
  CurrencySymbol →
  Value →
  { initTokenStatusData ∷ Plutus.Map.Map TokenName BigInt }
initTokenStatus sym =
  Value.getValue
    >>> Plutus.Map.lookup sym
    >>> fromMaybe Plutus.Map.empty
    >>> { initTokenStatusData: _ }
