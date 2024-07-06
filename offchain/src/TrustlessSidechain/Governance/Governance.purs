module TrustlessSidechain.Governance.Governance (Governance(..)) where

import Contract.Prelude
import TrustlessSidechain.Governance.MultiSig (MultiSigGovParams)
import TrustlessSidechain.Governance.Admin (GovernanceAuthority)

data Governance
    = MultiSig MultiSigGovParams
    | Admin GovernanceAuthority

instance Show Governance where
    show (MultiSig params) = "MultiSig " <> show params
    show (Admin authority) = "Admin " <> show authority