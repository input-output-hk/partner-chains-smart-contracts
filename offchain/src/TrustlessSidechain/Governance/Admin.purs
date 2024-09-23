module TrustlessSidechain.Governance.Admin
  ( GovernanceAuthority(GovernanceAuthority)
  , mkGovernanceAuthority
  , governanceAuthorityLookupsAndConstraints
  ) where

import Contract.Prelude

import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Contract.PlutusData (class FromData, class ToData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints

newtype GovernanceAuthority = GovernanceAuthority PaymentPubKeyHash

derive instance Newtype GovernanceAuthority _
derive instance Generic GovernanceAuthority _
derive newtype instance ToData GovernanceAuthority
derive newtype instance FromData GovernanceAuthority

instance Show GovernanceAuthority where
  show = genericShow

derive newtype instance Eq GovernanceAuthority

mkGovernanceAuthority :: PaymentPubKeyHash -> GovernanceAuthority
mkGovernanceAuthority = GovernanceAuthority

governanceAuthorityLookupsAndConstraints ::
  GovernanceAuthority ->

  { lookups :: Lookups.ScriptLookups
  , constraints :: Constraints.TxConstraints
  }
governanceAuthorityLookupsAndConstraints (GovernanceAuthority pkh) = do
  let
    lookups = Lookups.ownPaymentPubKeyHash pkh
    constraints = Constraints.mustBeSignedBy pkh
  { lookups, constraints }
