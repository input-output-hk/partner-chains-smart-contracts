{-# LANGUAGE ScopedTypeVariables #-}

module TrustlessSidechain.Governance (
  GovernanceAuthority (GovernanceAuthority),
  isApprovedBy,
  isApprovedByUnsafe,
  mkGovernanceAuthority,
) where

import Plutus.V2.Ledger.Api (PubKeyHash, TxInfo)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types
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
