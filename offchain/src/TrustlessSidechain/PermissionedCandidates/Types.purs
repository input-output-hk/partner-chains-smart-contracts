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
  ( productFromData4
  , productToData4
  )

data PermissionedCandidatesValidatorRedeemer
  = UpdatePermissionedCandidates
  | RemovePermissionedCandidates

instance ToData PermissionedCandidatesValidatorRedeemer where
  toData UpdatePermissionedCandidates = Integer (BigInt.fromInt 0)
  toData RemovePermissionedCandidates = Integer (BigInt.fromInt 1)

instance FromData PermissionedCandidatesValidatorRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ UpdatePermissionedCandidates
    | x == BigInt.fromInt 1 = Just $ RemovePermissionedCandidates
  fromData _ = Nothing

data PermissionedCandidateKeys = PermissionedCandidateKeys
  { mainchainKey ∷ ByteArray
  , sidechainKey ∷ ByteArray
  , auraKey ∷ ByteArray
  , grandpaKey ∷ ByteArray
  }

derive instance Eq PermissionedCandidateKeys

instance ToData PermissionedCandidateKeys where
  toData
    ( PermissionedCandidateKeys
        { mainchainKey, sidechainKey, auraKey, grandpaKey }
    ) =
    productToData4 mainchainKey sidechainKey auraKey grandpaKey

instance FromData PermissionedCandidateKeys where
  fromData =
    productFromData4 $
      \mainchainKey sidechainKey auraKey grandpaKey →
        PermissionedCandidateKeys
          { mainchainKey, sidechainKey, auraKey, grandpaKey }

data PermissionedCandidatesValidatorDatum = PermissionedCandidatesValidatorDatum
  { candidates ∷ Array PermissionedCandidateKeys
  }

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

instance ToData PermissionedCandidatesPolicyRedeemer where
  toData PermissionedCandidatesMint = Integer (BigInt.fromInt 0)
  toData PermissionedCandidatesBurn = Integer (BigInt.fromInt 1)

instance FromData PermissionedCandidatesPolicyRedeemer where
  fromData (Integer x)
    | x == BigInt.fromInt 0 = Just $ PermissionedCandidatesMint
    | x == BigInt.fromInt 1 = Just $ PermissionedCandidatesBurn
  fromData _ = Nothing
