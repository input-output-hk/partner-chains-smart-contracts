module TrustlessSidechain.PermissionedCandidates.Types
  ( PermissionedCandidatesValidatorRedeemer
      ( UpdatePermissionedCandidates
      , RemovePermissionedCandidates
      )
  , PermissionedCandidatesValidatorDatum(PermissionedCandidatesValidatorDatum)
  , PermissionedCandidatesPolicyRedeemer
      ( PermissionedCandidatesMint
      , PermissionedCandidatesBurn
      )
  , PermissionedCandidateKeys(PermissionedCandidateKeys)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Data.BigInt as BigInt
import TrustlessSidechain.Utils.Data
  ( productFromData3
  , productToData3
  )

data PermissionedCandidatesValidatorRedeemer
  = UpdatePermissionedCandidates
  | RemovePermissionedCandidates

derive instance Eq PermissionedCandidatesValidatorRedeemer

derive instance Generic PermissionedCandidatesValidatorRedeemer _

instance Show PermissionedCandidatesValidatorRedeemer where
  show = genericShow

instance ToData PermissionedCandidatesValidatorRedeemer where
  toData UpdatePermissionedCandidates = Integer (BigInt.fromInt 0)
  toData RemovePermissionedCandidates = Integer (BigInt.fromInt 1)

instance FromData PermissionedCandidatesValidatorRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ UpdatePermissionedCandidates
    | x == BigInt.fromInt 1 = Just $ RemovePermissionedCandidates
  fromData _ = Nothing

data PermissionedCandidateKeys = PermissionedCandidateKeys
  { sidechainKey ∷ ByteArray
  , auraKey ∷ ByteArray
  , grandpaKey ∷ ByteArray
  }

derive instance Eq PermissionedCandidateKeys
derive instance Ord PermissionedCandidateKeys

derive instance Generic PermissionedCandidateKeys _

instance Show PermissionedCandidateKeys where
  show = genericShow

instance ToData PermissionedCandidateKeys where
  toData
    ( PermissionedCandidateKeys
        { sidechainKey, auraKey, grandpaKey }
    ) =
    productToData3 sidechainKey auraKey grandpaKey

instance FromData PermissionedCandidateKeys where
  fromData =
    productFromData3 $
      \sidechainKey auraKey grandpaKey →
        PermissionedCandidateKeys
          { sidechainKey, auraKey, grandpaKey }

data PermissionedCandidatesValidatorDatum = PermissionedCandidatesValidatorDatum
  { candidates ∷ Array PermissionedCandidateKeys
  }

derive instance Eq PermissionedCandidatesValidatorDatum

derive instance Generic PermissionedCandidatesValidatorDatum _

instance Show PermissionedCandidatesValidatorDatum where
  show = genericShow

instance ToData PermissionedCandidatesValidatorDatum where
  toData (PermissionedCandidatesValidatorDatum { candidates }) = toData
    candidates

instance FromData PermissionedCandidatesValidatorDatum where
  fromData x = do
    candidates ← fromData x
    pure $ PermissionedCandidatesValidatorDatum { candidates }

data PermissionedCandidatesPolicyRedeemer
  = PermissionedCandidatesMint
  | PermissionedCandidatesBurn

derive instance Eq PermissionedCandidatesPolicyRedeemer

derive instance Generic PermissionedCandidatesPolicyRedeemer _

instance Show PermissionedCandidatesPolicyRedeemer where
  show = genericShow

instance ToData PermissionedCandidatesPolicyRedeemer where
  toData PermissionedCandidatesMint = Integer (BigInt.fromInt 0)
  toData PermissionedCandidatesBurn = Integer (BigInt.fromInt 1)

instance FromData PermissionedCandidatesPolicyRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ PermissionedCandidatesMint
    | x == BigInt.fromInt 1 = Just $ PermissionedCandidatesBurn
  fromData _ = Nothing
