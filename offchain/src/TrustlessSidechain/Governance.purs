-- | Module for common governance definitions.
module TrustlessSidechain.Governance
  ( Governance(..)
  ) where

import Contract.Prelude

import TrustlessSidechain.Governance.MultiSig (MultiSigGovParams)

-- | Available forms of governance
data Governance = MultiSig MultiSigGovParams

instance Show Governance where
  show (MultiSig params) = "MultiSig " <> show params
