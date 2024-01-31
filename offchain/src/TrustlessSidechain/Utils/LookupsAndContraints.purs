module TrustlessSidechain.Utils.LookupsAndConstraints
  ( mintOneToken
  , burnOneToken
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( Redeemer
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import TrustlessSidechain.Types (CurrencyInfo)

-- | Build lookups and constraints to mint one token of a given name for a
-- | provided currency.
mintOneToken ∷
  TokenName →
  Redeemer →
  CurrencyInfo →
  { lookups ∷ ScriptLookups Void
  , constraints ∷ TxConstraints Void Void
  }
mintOneToken = oneTokenHelper one

-- | Build lookups and constraints to burn one token of a given name for a
-- | provided currency.
burnOneToken ∷
  TokenName →
  Redeemer →
  CurrencyInfo →
  { lookups ∷ ScriptLookups Void
  , constraints ∷ TxConstraints Void Void
  }
burnOneToken = oneTokenHelper (negate one)

-- | Worker for `mintOneToken` and `burnOneToken`.
oneTokenHelper ∷
  BigInt →
  TokenName →
  Redeemer →
  CurrencyInfo →
  { lookups ∷ ScriptLookups Void
  , constraints ∷ TxConstraints Void Void
  }
oneTokenHelper amount tn redeemer { currencySymbol, mintingPolicy } =
  { lookups: Lookups.mintingPolicy mintingPolicy
  , constraints: Constraints.mustMintValueWithRedeemer
      redeemer
      (Value.singleton currencySymbol tn amount)
  }
