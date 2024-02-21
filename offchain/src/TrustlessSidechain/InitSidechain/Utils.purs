module TrustlessSidechain.InitSidechain.Utils
  ( initTokenCurrencyInfo
  , mintOneInitToken
  , burnOneInitToken
  , getInitTokenInfo
  ) where

import Contract.Prelude

import Contract.AssocMap as AssocMap
import Contract.Monad (Contract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.Value (TokenName, getValue)
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenRedeemer(MintInitToken, BurnInitToken)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address
  ( getCurrencyInfo
  )
import TrustlessSidechain.Utils.LookupsAndConstraints
  ( burnOneToken
  , mintOneToken
  )
import TrustlessSidechain.Utils.Utxos (getOwnUTxOsTotalValue)
import TrustlessSidechain.Versioning.Types
  ( ScriptId(InitTokenPolicy)
  )

-- | `initTokenCurrencyInfo` gets the minting policy and currency symbol
-- | corresponding to `InitTokenPolicy`.
initTokenCurrencyInfo ∷
  SidechainParams →
  Contract CurrencyInfo
initTokenCurrencyInfo sp =
  getCurrencyInfo InitTokenPolicy [ toData sp ]

-- | Build lookups and constraints to mint one initialisation token of a
-- | specified name.
mintOneInitToken ∷
  SidechainParams →
  TokenName →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mintOneInitToken sp tn =
  mintOneToken tn (Redeemer $ toData MintInitToken) <$> initTokenCurrencyInfo sp

-- | Build lookups and constraints to burn one initialisation token of a
-- | specified name.
burnOneInitToken ∷
  SidechainParams →
  TokenName →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
burnOneInitToken sp tn =
  burnOneToken tn (Redeemer $ toData BurnInitToken) <$> initTokenCurrencyInfo sp

getInitTokenInfo ∷ SidechainParams → Contract Unit
getInitTokenInfo sidechainParams = do
  -- Grab UTxOs we have at our own wallet
  ownValue ← getOwnUTxOsTotalValue

  -- Take only the init tokens
  { currencySymbol } ← initTokenCurrencyInfo sidechainParams

  case AssocMap.lookup currencySymbol (getValue ownValue) of
    Nothing → pure unit
    Just _ → pure unit
