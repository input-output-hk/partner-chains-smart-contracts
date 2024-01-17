module TrustlessSidechain.CommitteeOraclePolicy
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , committeeOraclePolicy
  , committeeOracleAssetClass
  , committeeOracleTn
  , committeeOracleCurrencyInfo
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData, fromData, toData)
import Contract.Prim.ByteArray as ByteArray
import Contract.Transaction (TransactionInput)
import Contract.Value as Value
import Partial.Unsafe (unsafePartial)
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (AssetClass, CurrencyInfo, assetClass)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CommitteeOraclePolicy)
  )

-- | `InitCommitteeHashMint` parameterizes the minting policy which identifies
-- | the utxo with the update committee hash validator script.
newtype InitCommitteeHashMint = InitCommitteeHashMint
  { icTxOutRef ∷ TransactionInput }

derive instance Eq InitCommitteeHashMint

derive instance Generic InitCommitteeHashMint _

instance Show InitCommitteeHashMint where
  show = genericShow

derive instance Newtype InitCommitteeHashMint _

instance ToData InitCommitteeHashMint where
  toData (InitCommitteeHashMint { icTxOutRef }) =
    toData icTxOutRef

instance FromData InitCommitteeHashMint where
  fromData = map (InitCommitteeHashMint <<< { icTxOutRef: _ }) <$> fromData

committeeOraclePolicy ∷ InitCommitteeHashMint → Contract CurrencyInfo
committeeOraclePolicy ichm =
  getCurrencyInfo CommitteeOraclePolicy [ toData ichm ]

-- | `committeeOracleAssetClass` is the asset class. See `committeeOracleTn`
-- | for details on the token name
committeeOracleAssetClass ∷ InitCommitteeHashMint → Contract AssetClass
committeeOracleAssetClass ichm = do
  { currencySymbol } ← committeeOraclePolicy ichm
  pure $ assetClass currencySymbol committeeOracleTn

-- | `committeeOracleTn` is the token name of the NFT which identifies
-- | the utxo which contains the committee hash. We use an empty bytestring for
-- | this because the name really doesn't matter, so we mighaswell save a few
-- | bytes by giving it the empty name.
committeeOracleTn ∷ Value.TokenName
committeeOracleTn = unsafePartial $ fromJust $ Value.mkTokenName $
  ByteArray.hexToByteArrayUnsafe ""

-- | Wrapper around `committeeOraclePolicy` that accepts `SidechainParams`.
committeeOracleCurrencyInfo ∷
  SidechainParams →
  Contract CurrencyInfo
committeeOracleCurrencyInfo (SidechainParams sp) = do
  committeeOraclePolicy $ InitCommitteeHashMint { icTxOutRef: sp.genesisUtxo }
