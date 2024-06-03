module TrustlessSidechain.CommitteeOraclePolicy
  ( committeeOracleTn
  , committeeOracleCurrencyInfo
  , mintOneCommitteeOracleInitToken
  , burnOneCommitteeOracleInitToken
  , committeeOracleInitTokenName
  ) where

import Contract.Prelude

import Cardano.Types.AssetName (AssetName)
import TrustlessSidechain.Utils.Asset (unsafeMkAssetName, emptyAssetName)
import Cardano.ToData (toData)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( burnOneInitToken
  , initTokenCurrencyInfo
  , mintOneInitToken
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CommitteeOraclePolicy)
  )
import Type.Row (type (+))

committeeOracleCurrencyInfo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) CurrencyInfo
committeeOracleCurrencyInfo sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: committeeOracleInitTokenName
      }
  getCurrencyInfo CommitteeOraclePolicy [ toData itac ]

committeeOracleInitTokenName ∷ AssetName
committeeOracleInitTokenName = unsafeMkAssetName "Committee oracle InitToken"

-- | `committeeOracleTn` is the token name of the NFT which identifies
-- | the utxo which contains the committee hash. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
committeeOracleTn ∷ AssetName
committeeOracleTn = emptyAssetName

-- | Build lookups and constraints to mint committee oracle initialization
-- | token.
mintOneCommitteeOracleInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mintOneCommitteeOracleInitToken sp =
  mintOneInitToken sp committeeOracleInitTokenName

-- | Build lookups and constraints to burn committee oracle initialization
-- | token.
burnOneCommitteeOracleInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
burnOneCommitteeOracleInitToken sp =
  burnOneInitToken sp committeeOracleInitTokenName
