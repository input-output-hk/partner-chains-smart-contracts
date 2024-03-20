module TrustlessSidechain.Governance
  ( GovernanceAuthority(GovernanceAuthority)
  , mkGovernanceAuthority
  , governanceAuthorityLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Address (PubKeyHash)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints

newtype GovernanceAuthority = GovernanceAuthority PubKeyHash

derive instance Newtype GovernanceAuthority _
derive instance Generic GovernanceAuthority _
derive newtype instance ToData GovernanceAuthority
derive newtype instance FromData GovernanceAuthority

instance Show GovernanceAuthority where
  show = genericShow

derive newtype instance Eq GovernanceAuthority

mkGovernanceAuthority ∷ PubKeyHash → GovernanceAuthority
mkGovernanceAuthority = GovernanceAuthority

governanceAuthorityLookupsAndConstraints ∷
  GovernanceAuthority →

  { lookups ∷ Lookups.ScriptLookups Void
  , constraints ∷ Constraints.TxConstraints Void Void
  }
governanceAuthorityLookupsAndConstraints (GovernanceAuthority pkh) = do
  let
    lookups = Lookups.ownPaymentPubKeyHash (wrap pkh)
    constraints = Constraints.mustBeSignedBy (wrap pkh)
  { lookups, constraints }
