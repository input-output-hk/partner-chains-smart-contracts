module TrustlessSidechain.Governance (
  approvedByGovernance,
) where

import Plutus.V1.Ledger.Value
import PlutusTx.AssocMap (lookup, toList)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Versioning

-- | Check whether a given transaction is approved by sidechain governance.  The
-- actual check is delegated to a governance minting policy stored in the
-- versioning system.  Caller specifies the requested governance version.  The
-- transaction must mint at least one token of the governance minting policy to
-- signify transaction approval.
{-# INLINEABLE approvedByGovernance #-}
approvedByGovernance
  :: VersionOracleConfig
  -> Integer -- ^ Governance version
  -> Unsafe.ScriptContext
  -> Bool
approvedByGovernance voc version ctx =
  case ofGovernanceCs of
    Just [(_, amount)] | amount > 0 -> True -- must mint at least one token, any name
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
