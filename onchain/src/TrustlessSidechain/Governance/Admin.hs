{-# LANGUAGE ScopedTypeVariables #-}

-- | Legacy governance mechanism.  This implementation was intended as a
-- placeholder until we get a proper governance mechanism in place.  A proper
-- governance mechanism is now live, but infortunatelly this piece of crap here
-- is used in the scripts used by customers and thus cannot be simply removed
-- without a proper migration strategy.
module TrustlessSidechain.Governance.Admin (
  GovernanceAuthority (GovernanceAuthority),
  isApprovedByAdmin,
  isApprovedByAdminUnsafe,
  mkGovernanceAuthority,
) where

import PlutusLedgerApi.V2 (PubKeyHash, TxInfo)
import PlutusLedgerApi.V2.Contexts (txSignedBy)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

{-# INLINEABLE mkGovernanceAuthority #-}
mkGovernanceAuthority :: PubKeyHash -> GovernanceAuthority
mkGovernanceAuthority = GovernanceAuthority

{-# INLINEABLE isApprovedByAdmin #-}
isApprovedByAdmin :: TxInfo -> GovernanceAuthority -> Bool
isApprovedByAdmin txInfo (GovernanceAuthority pkh) = txSignedBy txInfo pkh

{-# INLINEABLE isApprovedByAdminUnsafe #-}
isApprovedByAdminUnsafe :: Unsafe.TxInfo -> GovernanceAuthority -> Bool
isApprovedByAdminUnsafe txInfo (GovernanceAuthority pkh) = Unsafe.txSignedBy txInfo pkh
