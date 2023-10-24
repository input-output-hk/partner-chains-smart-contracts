module TrustlessSidechain.DParameter.Types
  ( DParameterValidatorRedeemer(UpdateDParameter, RemoveDParameter)
  , DParameterValidatorDatum(DParameterValidatorDatum)
  , DParameterPolicyRedeemer(DParameterMint, DParameterBurn)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer)
  )
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productToData2
  )

data DParameterValidatorRedeemer
  = UpdateDParameter
  | RemoveDParameter

instance ToData DParameterValidatorRedeemer where
  toData UpdateDParameter = Integer (BigInt.fromInt 0)
  toData RemoveDParameter = Integer (BigInt.fromInt 1)

instance FromData DParameterValidatorRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ UpdateDParameter
    | x == BigInt.fromInt 1 = Just $ RemoveDParameter
  fromData _ = Nothing

data DParameterValidatorDatum = DParameterValidatorDatum
  { permissionedCandidatesCount ∷ BigInt
  , registeredCandidatesCount ∷ BigInt
  }

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

data DParameterPolicyRedeemer
  = DParameterMint
  | DParameterBurn

instance ToData DParameterPolicyRedeemer where
  toData DParameterMint = Integer (BigInt.fromInt 0)
  toData DParameterBurn = Integer (BigInt.fromInt 1)

instance FromData DParameterPolicyRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ DParameterMint
    | x == BigInt.fromInt 1 = Just $ DParameterBurn
  fromData _ = Nothing
