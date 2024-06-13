module TrustlessSidechain.Governance (
  approvedByGovernance,
) where

import Plutus.V1.Ledger.Value
import PlutusTx.AssocMap (lookup, toList)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Versioning

-- | This function will be moved to a governance module in the future
{-# INLINEABLE approvedByGovernance #-}
approvedByGovernance
  :: VersionOracleConfig
  -> Integer -- ^ Governance version
  -> Unsafe.ScriptContext
  -> Bool
approvedByGovernance voc version ctx =
  flip (maybe False) ofGovernanceCs $ \case
    [(_, amount)] | amount > 0 -> True -- must mint at least one token, any name
    _ -> False
  where
    ofGovernanceCs :: Maybe [(TokenName, Integer)]
    ofGovernanceCs =
      fmap toList . lookup governanceTokenCurrencySymbol . getValue $ minted

    governanceTokenCurrencySymbol :: CurrencySymbol
    governanceTokenCurrencySymbol =
      getVersionedCurrencySymbolUnsafe
        voc
        (VersionOracle {version, scriptId = governancePolicyId})
        ctx

    minted :: Value
    minted = Unsafe.decode . Unsafe.txInfoMint . Unsafe.scriptContextTxInfo $ ctx
