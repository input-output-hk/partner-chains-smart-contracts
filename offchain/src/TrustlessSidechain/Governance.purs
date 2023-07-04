module TrustlessSidechain.Governance
  ( GovernanceAuthority
  , mkGovernanceAuthority
  , governanceAuthorityCodec
  , governanceAuthorityLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Address (PubKeyHash)
import Contract.Monad (Contract)
import Contract.PlutusData (class FromData, class ToData, fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Data.Codec.Argonaut as CA
import TrustlessSidechain.Utils.Codecs (pubKeyHashCodec)

newtype GovernanceAuthority = GovernanceAuthority PubKeyHash

instance Show GovernanceAuthority where
  show (GovernanceAuthority pkh) = show pkh

instance ToData GovernanceAuthority where
  toData (GovernanceAuthority pkh) = toData pkh

instance FromData GovernanceAuthority where
  fromData pkh = GovernanceAuthority <$> fromData pkh

-- | JSON codec for PubKeyHash.
governanceAuthorityCodec ∷ CA.JsonCodec GovernanceAuthority
governanceAuthorityCodec = CA.prismaticCodec "GovernanceAuthority"
  (Just <<< mkGovernanceAuthority)
  (\(GovernanceAuthority ga) → ga)
  pubKeyHashCodec

mkGovernanceAuthority ∷ PubKeyHash → GovernanceAuthority
mkGovernanceAuthority = GovernanceAuthority

governanceAuthorityLookupsAndConstraints ∷
  GovernanceAuthority →
  Contract
    { lookups ∷ Lookups.ScriptLookups Void
    , constraints ∷ Constraints.TxConstraints Void Void
    }
governanceAuthorityLookupsAndConstraints (GovernanceAuthority pkh) = do
  let
    lookups = Lookups.ownPaymentPubKeyHash (wrap pkh)
    constraints = Constraints.mustBeSignedBy (wrap pkh)
  pure { lookups, constraints }
