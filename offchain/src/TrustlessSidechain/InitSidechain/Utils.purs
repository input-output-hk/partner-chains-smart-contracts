module TrustlessSidechain.InitSidechain.Utils
  ( initTokenCurrencyInfo
  , mintOneInitToken
  , burnOneInitToken
  ) where

import Contract.Prelude

import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Cardano.ToData (toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.Value (TokenName)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
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
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(InitTokenPolicy)
  )
import Type.Row (type (+))

-- | `initTokenCurrencyInfo` gets the minting policy and currency symbol
-- | corresponding to `InitTokenPolicy`.
initTokenCurrencyInfo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) CurrencyInfo
initTokenCurrencyInfo sp =
  getCurrencyInfo InitTokenPolicy [ toData sp ]

-- | Build lookups and constraints to mint one initialisation token of a
-- | specified name.
mintOneInitToken ∷
  ∀ r.
  SidechainParams →
  TokenName →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mintOneInitToken sp tn =
  mintOneToken tn (RedeemerDatum $ toData MintInitToken) <$> initTokenCurrencyInfo sp

-- | Build lookups and constraints to burn one initialisation token of a
-- | specified name.
burnOneInitToken ∷
  ∀ r.
  SidechainParams →
  TokenName →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
burnOneInitToken sp tn =
  burnOneToken tn (RedeemerDatum $ toData BurnInitToken) <$> initTokenCurrencyInfo sp
