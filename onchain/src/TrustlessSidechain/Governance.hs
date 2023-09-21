{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.Governance (
  GovernanceAuthority,
  isApprovedBy,
  mkGovernanceAuthority,
) where

import Plutus.V2.Ledger.Api (PubKeyHash, TxInfo)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import PlutusTx (makeLift)
import TrustlessSidechain.HaskellPrelude qualified as Prelude
import TrustlessSidechain.PlutusPrelude

newtype GovernanceAuthority = GovernanceAuthority PubKeyHash
  deriving newtype (Prelude.Eq, Prelude.Ord, ToData, FromData, UnsafeFromData)

PlutusTx.makeLift ''GovernanceAuthority

{-# INLINEABLE mkGovernanceAuthority #-}
mkGovernanceAuthority :: PubKeyHash -> GovernanceAuthority
mkGovernanceAuthority = GovernanceAuthority

{-# INLINEABLE isApprovedBy #-}
isApprovedBy :: TxInfo -> GovernanceAuthority -> Bool
isApprovedBy txInfo (GovernanceAuthority pkh) = txSignedBy txInfo pkh
