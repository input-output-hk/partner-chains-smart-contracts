module TrustlessSidechain.CommitteeOraclePolicy
  ( committeeOracleAssetClass
  , committeeOracleTn
  , committeeOracleCurrencyInfo
  , mintOneCommitteeOracleInitToken
  , burnOneCommitteeOracleInitToken
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray as ByteArray
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
import TrustlessSidechain.Types (AssetClass, CurrencyInfo, assetClass)
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

-- | `committeeOracleAssetClass` is the asset class. See `committeeOracleTn`
-- | for details on the token name
committeeOracleAssetClass ∷ SidechainParams → Contract AssetClass
committeeOracleAssetClass sp = do
  { currencySymbol } ← committeeOracleCurrencyInfo sp
  pure $ assetClass currencySymbol committeeOracleTn

committeeOracleInitTokenName ∷ Value.TokenName
committeeOracleInitTokenName = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe "Committee oracle InitToken"

-- | `committeeOracleTn` is the token name of the NFT which identifies
-- | the utxo which contains the committee hash. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
committeeOracleTn ∷ Value.TokenName
committeeOracleTn = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe ""

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
