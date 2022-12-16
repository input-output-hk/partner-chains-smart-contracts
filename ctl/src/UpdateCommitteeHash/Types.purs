-- | `UpdateCommitteeHash.Types` contains the types relating to the update
-- | committee hash endpoint.
-- |
-- | Note: the reason for the existence of this module is because without this
-- | there are some cyclic dependencies between `MPTRoot` and `UpdateCommitteeHash`
module UpdateCommitteeHash.Types
  ( UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  , UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  , UpdateCommitteeHashMessage(UpdateCommitteeHashMessage)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(..)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import SidechainParams (SidechainParams, SidechainParams')
import Types (AssetClass, PubKey, Signature)

-- | `UpdateCommitteeHashDatum` is the datum for the update committee hash
-- | validator
newtype UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash ∷ ByteArray
  , sidechainEpoch ∷ BigInt
  }

derive instance Generic UpdateCommitteeHashDatum _
derive instance Newtype UpdateCommitteeHashDatum _
instance ToData UpdateCommitteeHashDatum where
  toData (UpdateCommitteeHashDatum { committeeHash, sidechainEpoch }) = Constr
    zero
    [ toData committeeHash, toData sidechainEpoch ]

instance FromData UpdateCommitteeHashDatum where
  fromData (Constr n [ a, b ])
    | n == zero =
        UpdateCommitteeHashDatum <$>
          ( { committeeHash: _, sidechainEpoch: _ }
              <$> fromData a
              <*> fromData b
          )
  fromData _ = Nothing

-- | `UpdateCommitteeHash` paramaterizes the the validator for the update
-- | committee hash policy.
newtype UpdateCommitteeHash = UpdateCommitteeHash
  { sidechainParams ∷ SidechainParams
  , uchAssetClass ∷ AssetClass
  , mptRootTokenCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic UpdateCommitteeHash _
derive instance Newtype UpdateCommitteeHash _
instance ToData UpdateCommitteeHash where
  toData
    ( UpdateCommitteeHash
        { sidechainParams, uchAssetClass, mptRootTokenCurrencySymbol }
    ) = Constr zero
    [ toData sidechainParams
    , toData uchAssetClass
    , toData mptRootTokenCurrencySymbol
    ]

-- | `InitCommitteeHashMint` parameterizes the minting policy which identifies
-- | the utxo with the update committee hash validator script.
newtype InitCommitteeHashMint = InitCommitteeHashMint
  { icTxOutRef ∷ TransactionInput }

derive instance Generic InitCommitteeHashMint _
derive instance Newtype InitCommitteeHashMint _
instance ToData InitCommitteeHashMint where
  toData (InitCommitteeHashMint { icTxOutRef }) =
    toData icTxOutRef

-- | `UpdateCommitteeHashRedeemer` is the redeemer for the update committee
-- | hash validator.
data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { committeeSignatures ∷ Array Signature
  , committeePubKeys ∷ Array PubKey
  , newCommitteePubKeys ∷ Array PubKey
  , previousMerkleRoot ∷ Maybe ByteArray
  }

derive instance Generic UpdateCommitteeHashRedeemer _
instance ToData UpdateCommitteeHashRedeemer where
  toData
    ( UpdateCommitteeHashRedeemer
        { committeeSignatures
        , committeePubKeys
        , newCommitteePubKeys
        , previousMerkleRoot
        }
    ) = Constr zero
    [ toData committeeSignatures
    , toData committeePubKeys
    , toData newCommitteePubKeys
    , toData previousMerkleRoot
    ]

-- | `UpdateCommitteeHashParams` is the offchain parameter for the update
-- | committee hash endpoint.
newtype UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { sidechainParams ∷ SidechainParams
  , newCommitteePubKeys ∷ Array PubKey
  , committeeSignatures ∷ Array (PubKey /\ Maybe Signature)
  , previousMerkleRoot ∷ Maybe ByteArray
  , sidechainEpoch ∷ BigInt -- sidechain epoch of the new committee
  }

derive newtype instance Show UpdateCommitteeHashParams
derive instance Newtype UpdateCommitteeHashParams _

-- | `UpdateCommitteeHashMessage` corresponds to the on chain type which is
-- | signed by the committee (technically, if `uchm` is an
-- | `UpdateCommitteeHashMessage`, then the committee signs
-- | `blake2b256Hash(serialiseToData (toBuiltinData uchm))`)
newtype UpdateCommitteeHashMessage = UpdateCommitteeHashMessage
  { sidechainParams ∷ SidechainParams'
  , -- `newCommitteePubKeys` is the new committee public keys and _should_
    -- be sorted lexicographically (recall that we can trust the bridge, so it
    -- should do this for us
    newCommitteePubKeys ∷ Array PubKey
  , previousMerkleRoot ∷ Maybe ByteArray
  , sidechainEpoch ∷ BigInt
  }

derive instance Generic UpdateCommitteeHashMessage _
instance ToData UpdateCommitteeHashMessage where
  toData
    ( UpdateCommitteeHashMessage
        { sidechainParams
        , newCommitteePubKeys
        , previousMerkleRoot
        , sidechainEpoch
        }
    ) = Constr zero
    [ toData sidechainParams
    , toData newCommitteePubKeys
    , toData previousMerkleRoot
    , toData sidechainEpoch
    ]
