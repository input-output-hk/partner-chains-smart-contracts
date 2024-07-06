module TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams (MultiSigGovParams)
  , multisigGovPolicy
  , multisigGovCurrencyInfo
  , multisigGovTokenName
  ) where

import Contract.Prelude

import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Cardano.Types.PlutusScript (PlutusScript)
import Contract.Value (TokenName)
import JS.BigInt as BigInt
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
-- @since Unreleased
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
    Constr (BigNum.fromInt 0)
      [ toData governanceMembers
      , toData requiredSignatures
      ]

instance FromData MultiSigGovParams where
  fromData = case _ of
    Constr ix [ gm, rs ] | (ix == BigNum.fromInt 0) → do
      governanceMembers ← fromData gm
      requiredSignatures ← fromData rs
      pure $ MultiSigGovParams
        { governanceMembers
        , requiredSignatures
        }
    _ → Nothing

instance Show MultiSigGovParams where
  show = genericShow

-- | A name for the checkpoint initialization token.  Must be unique among
-- | initialization tokens.
multisigGovTokenName ∷ TokenName
multisigGovTokenName = emptyAssetName

multisigGovPolicy ∷
  ∀ r.
  MultiSigGovParams →
  Run (EXCEPT OffchainError + r) PlutusScript
multisigGovPolicy msgp =
  mkMintingPolicyWithParams MultiSigPolicy [ toData msgp ]

-- | Get currency information for a multisignature governance policy.
multisigGovCurrencyInfo ∷
  ∀ r.
  MultiSigGovParams →
  Run (EXCEPT OffchainError + r) CurrencyInfo
multisigGovCurrencyInfo msgp = do
  getCurrencyInfo MultiSigPolicy [ toData msgp ]
