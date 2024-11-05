module TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  , multisigGovPolicy
  , multisigGovCurrencyInfo
  , multisigGovTokenName
  , multisigLookupsAndConstraints
  ) where

import Contract.Prelude

import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash(PaymentPubKeyHash))
import Cardano.Types.PlutusScript (PlutusScript)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(List)
  , fromData
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints
  ( TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (TokenName)
import Data.Array as Array
import Data.Maybe as Maybe
import JS.BigInt as BigInt
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Utils.Asset (emptyAssetName)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(MultiSigPolicy)
  )
import Type.Row (type (+))

-- | Parameters of the security mechanism.  Note that setting
-- `requiredSignatures` to a value greater than `length governanceMembers` will
-- result in governance that can never approve anything.
--
-- NOTE: the order of entries in the `governanceMembers` matters!  Since
-- `MultiSigGovParams` is used to parameterize the multi-sognature governance
-- minting policy, changing the order of elements will change the hash of the
-- policy.
--
-- @since v6.1.0
newtype MultiSigGovParams = MultiSigGovParams
  { -- | Members of the governance
    governanceMembers :: Array Ed25519KeyHash
  -- | Minimal required number of signatures
  , requiredSignatures :: BigInt.BigInt
  }

derive instance Generic MultiSigGovParams _

derive instance Newtype MultiSigGovParams _

derive newtype instance Eq MultiSigGovParams

instance ToData MultiSigGovParams where
  toData
    ( MultiSigGovParams
        { governanceMembers
        , requiredSignatures
        }
    ) =
    List
      [ toData governanceMembers
      , toData requiredSignatures
      ]

instance FromData MultiSigGovParams where
  fromData = case _ of
    List [ gm, rs ] -> do
      governanceMembers <- fromData gm
      requiredSignatures <- fromData rs
      pure $ MultiSigGovParams
        { governanceMembers
        , requiredSignatures
        }
    _ -> Nothing

instance Show MultiSigGovParams where
  show = genericShow

-- | A name for the multiSigGov initialization token.  Must be unique among
-- | initialization tokens.
multisigGovTokenName :: TokenName
multisigGovTokenName = emptyAssetName

multisigGovPolicy ::
  forall r.
  MultiSigGovParams ->
  Run (EXCEPT OffchainError + r) PlutusScript
multisigGovPolicy msgp =
  mkMintingPolicyWithParams MultiSigPolicy [ toData msgp ]

-- | Get currency information for a multisignature governance policy.
multisigGovCurrencyInfo ::
  forall r.
  MultiSigGovParams ->
  Run (EXCEPT OffchainError + r) CurrencyInfo
multisigGovCurrencyInfo msgp = do
  getCurrencyInfo MultiSigPolicy [ toData msgp ]

-- TODO: proper implementation for multiple signatures collected at later stage
multisigLookupsAndConstraints ::
  MultiSigGovParams ->
  { lookups :: ScriptLookups
  , constraints :: TxConstraints
  }
multisigLookupsAndConstraints
  (MultiSigGovParams { governanceMembers }) = do
  let
    lookups = mempty
    constraints = Constraints.mustBeSignedBy $ PaymentPubKeyHash $ unsafePartial
      $ Maybe.fromJust (Array.head governanceMembers)
  { lookups, constraints }
