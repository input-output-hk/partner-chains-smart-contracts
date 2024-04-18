module TrustlessSidechain.DataStorage.Types
  ( DataStorageValidatorDatum(DataStorageValidatorDatum)
  ) where

import Contract.Prelude

import Contract.PlutusData (class FromData, class ToData, fromData, toData)

data DataStorageValidatorDatum r = DataStorageValidatorDatum r

derive instance (Eq r) ⇒ Eq (DataStorageValidatorDatum r)

derive instance Generic (DataStorageValidatorDatum r) _

instance (Show r) ⇒ Show (DataStorageValidatorDatum r) where
  show = genericShow

instance (ToData r) ⇒ ToData (DataStorageValidatorDatum r) where
  toData (DataStorageValidatorDatum r) = toData r

instance (FromData r) ⇒ FromData (DataStorageValidatorDatum r) where
  fromData = map DataStorageValidatorDatum <<< fromData