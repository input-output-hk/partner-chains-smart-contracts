module TrustlessSidechain.InitSidechain.Init
  ( getInitTokenStatus
  , getScriptsToInsert
  , init
  , insertScriptsIdempotent
  ) where

import Contract.Prelude

import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.Value (Value)
import Cardano.Types.Value as Value
import Contract.Transaction (TransactionHash)
import Data.List (List, filter)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSKinds)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
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
    Int → -- Version number
    Run (APP + r)
      { versionedPolicies ∷ List (Tuple ScriptId PlutusScript)
      , versionedValidators ∷ List (Tuple ScriptId PlutusScript)
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
    { versionedPolicies ∷ List (Tuple ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple ScriptId PlutusScript)
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
  { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
  , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
  } →
  Int →
  Run (APP + r)
    { versionedPolicies ∷ List (Tuple Types.ScriptId PlutusScript)
    , versionedValidators ∷ List (Tuple Types.ScriptId PlutusScript)
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
-- | init token exists. If it doesn't, return Nothing.
init ∷
  ∀ r.
  (String → SidechainParams → Run (APP + r) TransactionHash) →
  String →
  AssetName →
  SidechainParams →
  Run (APP + r) (Maybe TransactionHash)
init f op nm sp = do
  tokenExists ← map (Map.member nm <<< _.initTokenStatusData)
    (getInitTokenStatus sp)
  if tokenExists then Just <$> f op sp else pure Nothing

-- | Get the init token data for the own wallet. Used in InitTokenStatus
-- | endpoint.
getInitTokenStatus ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { initTokenStatusData ∷ Map AssetName BigNum }
getInitTokenStatus scParams = do
  { currencySymbol } ← initTokenCurrencyInfo scParams

  -- NOTE: If Value later exposes a way to filter by currency (or to `map` or
  -- `lookup`), save a little computation by doing that before combining in
  -- getOwnUTxOsTotalValue.
  map (initTokenStatus currencySymbol) getOwnUTxOsTotalValue

-- | Get the init token data for the given `CurrencySymbol` from a given
-- | `Value`. Used in the InitTokenStatus endpoint.
initTokenStatus ∷
  ScriptHash →
  Value →
  { initTokenStatusData ∷ Map AssetName BigNum }
initTokenStatus sym =
  Value.getMultiAsset
    >>> unwrap
    >>> Map.lookup sym
    >>> fromMaybe Map.empty
    >>> { initTokenStatusData: _ }
