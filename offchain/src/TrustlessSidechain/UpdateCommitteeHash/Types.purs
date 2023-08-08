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

import Contract.Address (Address)
import Contract.PlutusData
  ( class FromData
  , class ToData
  , fromData
  , toData
  )
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  )
import TrustlessSidechain.Utils.Data
  ( productFromData2
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

derive instance Generic UpdateCommitteeHashRedeemer _

derive instance Newtype UpdateCommitteeHashRedeemer _

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

derive instance Generic UpdateCommitteeHash _

derive instance Newtype UpdateCommitteeHash _

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

-- | `UpdateCommitteeHashParams` is the offchain parameter for the update
-- | committee hash endpoint.
newtype UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { sidechainParams ∷ SidechainParams
  , newCommitteePubKeys ∷ Array EcdsaSecp256k1PubKey
  , committeeSignatures ∷
      Array (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature)
  , previousMerkleRoot ∷ Maybe RootHash
  , sidechainEpoch ∷ BigInt -- sidechain epoch of the new committee
  }

derive newtype instance Show UpdateCommitteeHashParams

derive instance Newtype UpdateCommitteeHashParams _

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
  , validatorAddress ∷ Address
  }

instance
  ToData aggregatePubKeys ⇒
  ToData (UpdateCommitteeHashMessage aggregatePubKeys) where
  toData
    ( UpdateCommitteeHashMessage
        { sidechainParams
        , newAggregatePubKeys
        , previousMerkleRoot
        , sidechainEpoch
        , validatorAddress
        }
    ) = productToData5
    sidechainParams
    newAggregatePubKeys
    previousMerkleRoot
    sidechainEpoch
    validatorAddress
