-- | `UpdateCommitteeHash.Types` contains the types relating to the update
-- | committee hash endpoint.
-- |
-- | Note: the reason for the existence of this module is because without this
-- | there are some cyclic dependencies between `MerkleRoot` and `UpdateCommitteeHash`
module TrustlessSidechain.UpdateCommitteeHash.Types
  ( UpdateCommitteeDatum(UpdateCommitteeDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , fromData
  , toData
  )
import Contract.Scripts (ValidatorHash)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productFromData4
  , productFromData5
  , productToData2
  , productToData4
  , productToData5
  )

-- | `UpdateCommitteeDatum` is the datum for the update committee hash
-- | validator
-- | This corresponds to the onchain type. In the future, the
-- | `aggregatePubKeys` type that this is instantiated with may change as we
-- | implement different ATMS schemes.
newtype UpdateCommitteeDatum aggregatePubKeys = UpdateCommitteeDatum
  { aggregatePubKeys ∷ aggregatePubKeys
  , sidechainEpoch ∷ BigInt
  }

derive newtype instance (Eq a) ⇒ Eq (UpdateCommitteeDatum a)

derive instance Generic (UpdateCommitteeDatum a) _

instance (Show a) ⇒ Show (UpdateCommitteeDatum a) where
  show = genericShow

instance
  ToData aggregatePubKeys ⇒
  ToData (UpdateCommitteeDatum aggregatePubKeys) where
  toData (UpdateCommitteeDatum { aggregatePubKeys, sidechainEpoch }) =
    productToData2
      aggregatePubKeys
      sidechainEpoch

instance
  FromData aggregatePubKeys ⇒
  FromData (UpdateCommitteeDatum aggregatePubKeys) where
  fromData = productFromData2
    ( \x y → UpdateCommitteeDatum
        { aggregatePubKeys: x
        , sidechainEpoch: y
        }
    )

-- | `UpdateCommitteeHashRedeemer` is the redeemer for the update committee hash
-- | validator
-- | This corresponds to the onchain type.
newtype UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { previousMerkleRoot ∷ Maybe RootHash
  }

derive newtype instance Eq UpdateCommitteeHashRedeemer

derive instance Generic UpdateCommitteeHashRedeemer _

derive instance Newtype UpdateCommitteeHashRedeemer _

instance Show UpdateCommitteeHashRedeemer where
  show = genericShow

instance ToData UpdateCommitteeHashRedeemer where
  toData (UpdateCommitteeHashRedeemer { previousMerkleRoot }) = toData
    previousMerkleRoot

instance FromData UpdateCommitteeHashRedeemer where
  fromData d = UpdateCommitteeHashRedeemer <$>
    ({ previousMerkleRoot: _ } <$> fromData d)

-- | `UpdateCommitteeHash` paramaterizes the the validator for the update
-- | committee hash policy.
newtype UpdateCommitteeHash = UpdateCommitteeHash
  { sidechainParams ∷ SidechainParams
  , committeeOracleCurrencySymbol ∷ CurrencySymbol
  , committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
  , merkleRootTokenCurrencySymbol ∷ CurrencySymbol
  }

derive newtype instance Eq UpdateCommitteeHash

derive instance Generic UpdateCommitteeHash _

derive instance Newtype UpdateCommitteeHash _

instance Show UpdateCommitteeHash where
  show = genericShow

instance ToData UpdateCommitteeHash where
  toData
    ( UpdateCommitteeHash
        { sidechainParams
        , committeeOracleCurrencySymbol
        , committeeCertificateVerificationCurrencySymbol
        , merkleRootTokenCurrencySymbol
        }
    ) = productToData4 sidechainParams
    committeeOracleCurrencySymbol
    committeeCertificateVerificationCurrencySymbol
    merkleRootTokenCurrencySymbol

instance FromData UpdateCommitteeHash where
  fromData = productFromData4 $
    \sidechainParams
     committeeOracleCurrencySymbol
     committeeCertificateVerificationCurrencySymbol
     merkleRootTokenCurrencySymbol → UpdateCommitteeHash
      { sidechainParams
      , committeeOracleCurrencySymbol
      , committeeCertificateVerificationCurrencySymbol
      , merkleRootTokenCurrencySymbol
      }

-- | `UpdateCommitteeHashMessage` corresponds to the on chain type which is
-- | signed by the committee (technically, if `uchm` is an
-- | `UpdateCommitteeHashMessage`, then the committee signs
-- | `blake2b256Hash(serialiseToData (toBuiltinData uchm))`)
newtype UpdateCommitteeHashMessage aggregatePubKeys = UpdateCommitteeHashMessage
  { sidechainParams ∷ SidechainParams
  , -- `newAggregatePubKeys` is the new committee public keys and _should_
    -- be sorted lexicographically (recall that we can trust the bridge, so it
    -- should do this for us
    -- newAggregatePubKeys ∷ Array EcdsaSecp256k1PubKey
    -- TODO: fix the documentation here
    newAggregatePubKeys ∷ aggregatePubKeys
  , previousMerkleRoot ∷ Maybe RootHash
  , sidechainEpoch ∷ BigInt
  , validatorHash ∷ ValidatorHash
  }

derive newtype instance (Eq a) ⇒ Eq (UpdateCommitteeHashMessage a)

derive instance Generic (UpdateCommitteeHashMessage a) _

instance (Show a) ⇒ Show (UpdateCommitteeHashMessage a) where
  show = genericShow

instance
  ToData aggregatePubKeys ⇒
  ToData (UpdateCommitteeHashMessage aggregatePubKeys) where
  toData
    ( UpdateCommitteeHashMessage
        { sidechainParams
        , newAggregatePubKeys
        , previousMerkleRoot
        , sidechainEpoch
        , validatorHash
        }
    ) = productToData5
    sidechainParams
    newAggregatePubKeys
    previousMerkleRoot
    sidechainEpoch
    validatorHash

instance
  ( FromData aggregatePubKeys
  ) ⇒
  FromData (UpdateCommitteeHashMessage aggregatePubKeys) where
  fromData = productFromData5 $
    \sidechainParams
     newAggregatePubKeys
     previousMerkleRoot
     sidechainEpoch
     validatorHash →
      UpdateCommitteeHashMessage
        { sidechainParams
        , newAggregatePubKeys
        , previousMerkleRoot
        , sidechainEpoch
        , validatorHash
        }
