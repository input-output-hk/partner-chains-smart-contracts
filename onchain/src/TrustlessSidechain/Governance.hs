{-# LANGUAGE ScopedTypeVariables #-}

module TrustlessSidechain.Governance (
  GovernanceAuthority (GovernanceAuthority),
  isApprovedBy,
  isApprovedByUnsafe,
  mkGovernanceAuthority,
) where

import PlutusLedgerApi.V2 (PubKeyHash, TxInfo)
import PlutusLedgerApi.V2.Contexts (txSignedBy)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (GovernanceAuthority (..))
import TrustlessSidechain.Types.Unsafe qualified as Unsafe

{-# INLINEABLE mkGovernanceAuthority #-}
mkGovernanceAuthority :: PubKeyHash -> GovernanceAuthority
mkGovernanceAuthority = GovernanceAuthority

{-# INLINEABLE isApprovedBy #-}
isApprovedBy :: TxInfo -> GovernanceAuthority -> Bool
isApprovedBy txInfo (GovernanceAuthority pkh) = txSignedBy txInfo pkh

{-# INLINEABLE isApprovedByUnsafe #-}
isApprovedByUnsafe :: Unsafe.TxInfo -> GovernanceAuthority -> Bool
isApprovedByUnsafe txInfo (GovernanceAuthority pkh) = Unsafe.txSignedBy txInfo pkh
