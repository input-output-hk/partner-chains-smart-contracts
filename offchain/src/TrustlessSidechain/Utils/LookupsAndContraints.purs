module TrustlessSidechain.Utils.LookupsAndConstraints
  ( mintOneToken
  , burnOneToken
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( Redeemer
  )
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Cardano.Types.AssetName (AssetName)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName)
import Contract.Value as Value
import TrustlessSidechain.Types (CurrencyInfo)
import Cardano.Types.Mint as Mint
import Cardano.Types.Int as Int

-- | Build lookups and constraints to mint one token of a given name for a
-- | provided currency.
mintOneToken ∷
  AssetName →
  RedeemerDatum →
  CurrencyInfo →
  { lookups ∷ ScriptLookups
  , constraints ∷ TxConstraints
  }
mintOneToken = oneTokenHelper Int.one

-- | Build lookups and constraints to burn one token of a given name for a
-- | provided currency.
burnOneToken ∷
  AssetName →
  RedeemerDatum →
  CurrencyInfo →
  { lookups ∷ ScriptLookups
  , constraints ∷ TxConstraints
  }
burnOneToken = oneTokenHelper (Int.negate Int.one)

-- | Worker for `mintOneToken` and `burnOneToken`.
oneTokenHelper ∷
  Int.Int →
  AssetName →
  RedeemerDatum →
  CurrencyInfo →
  { lookups ∷ ScriptLookups
  , constraints ∷ TxConstraints
  }
oneTokenHelper amount tn redeemer { currencySymbol, mintingPolicy } =
  { lookups: Lookups.plutusMintingPolicy mintingPolicy
  , constraints: Constraints.mustMintValueWithRedeemer
      redeemer
      (Mint.singleton currencySymbol tn amount)
  }
