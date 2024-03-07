module TrustlessSidechain.CommitteeOraclePolicy
  ( committeeOracleTn
  , committeeOracleCurrencyInfo
  , mintOneCommitteeOracleInitToken
  , burnOneCommitteeOracleInitToken
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints (TxConstraints)
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
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

committeeOracleCurrencyInfo ∷ SidechainParams → Contract CurrencyInfo
committeeOracleCurrencyInfo sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: committeeOracleInitTokenName
      }
  getCurrencyInfo CommitteeOraclePolicy [ toData itac ]

committeeOracleInitTokenName ∷ Value.TokenName
committeeOracleInitTokenName =
  unsafePartial $ fromJust $ Value.mkTokenName
    =<< byteArrayFromAscii "Committee oracle InitToken"

-- | `committeeOracleTn` is the token name of the NFT which identifies
-- | the utxo which contains the committee hash. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
committeeOracleTn ∷ Value.TokenName
committeeOracleTn =
  unsafePartial $ fromJust $ Value.mkTokenName
    =<< byteArrayFromAscii ""

-- | Build lookups and constraints to mint committee oracle initialization
-- | token.
mintOneCommitteeOracleInitToken ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mintOneCommitteeOracleInitToken sp =
  mintOneInitToken sp committeeOracleInitTokenName

-- | Build lookups and constraints to burn committee oracle initialization
-- | token.
burnOneCommitteeOracleInitToken ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
burnOneCommitteeOracleInitToken sp =
  burnOneInitToken sp committeeOracleInitTokenName
