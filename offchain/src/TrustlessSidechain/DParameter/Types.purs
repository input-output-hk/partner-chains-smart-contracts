module TrustlessSidechain.DParameter.Types
  ( DParameterValidatorDatum(DParameterValidatorDatum)
  ) where

import Contract.Prelude

import Cardano.Types (BigInt)
import Contract.PlutusData
  ( class FromData
  , class ToData
  )
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productToData2
  )

data DParameterValidatorDatum = DParameterValidatorDatum
  { permissionedCandidatesCount ∷ BigInt
  , registeredCandidatesCount ∷ BigInt
  }

derive instance Eq DParameterValidatorDatum

derive instance Generic DParameterValidatorDatum _

instance Show DParameterValidatorDatum where
  show = genericShow

instance ToData DParameterValidatorDatum where
  toData
    ( DParameterValidatorDatum
        { permissionedCandidatesCount
        , registeredCandidatesCount
        }
    ) =
    productToData2 permissionedCandidatesCount registeredCandidatesCount

instance FromData DParameterValidatorDatum where
  fromData =
    productFromData2 $ \permissionedCandidatesCount registeredCandidatesCount →
      DParameterValidatorDatum
        { permissionedCandidatesCount
        , registeredCandidatesCount
        }
