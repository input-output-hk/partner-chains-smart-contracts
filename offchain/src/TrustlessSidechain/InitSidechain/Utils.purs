module TrustlessSidechain.InitSidechain.Utils
  ( initTokenCurrencyInfo
  , mintOneInitToken
  , burnOneInitToken
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.Value (TokenName)
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
