-- | `MPTRoot.Types` contains types relating to the `MPTRoot` endpoint.
-- |
-- |  Note: the reason for the existence of this module is because without this
-- | module, there are some cyclic dependencies between `MPTRoot` and
-- | `UpdateCommitteeHash`.
module MPTRoot.Types
  ( SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  , SaveRootParams(SaveRootParams)
  , MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class ToData
  , PlutusData(..)
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol)
import SidechainParams (SidechainParams, SidechainParams')
import Utils.Crypto (SidechainPublicKey, SidechainSignature)

-- | `SignedMerkleRoot` is the redeemer for the minting policy.
data SignedMerkleRoot = SignedMerkleRoot
  { -- The new merkle root to insert.
    merkleRoot ∷ ByteArray
  , -- Either `Just` the last merkle root (in the case it exists), or `Nothing`
    -- if there is no such last merkle root (i.e., in the first transaction).
    previousMerkleRoot ∷ Maybe ByteArray
  , -- Ordered as their corresponding public keys. In the case that not all the
    -- committees' public keys signed the message, the length of this list will be
    -- less than the committee public keys.
    signatures ∷ Array SidechainSignature
  , -- Sorted public keys of all committee members
    committeePubKeys ∷ Array SidechainPublicKey
  }

derive instance Generic SignedMerkleRoot _
instance ToData SignedMerkleRoot where
  toData
    ( SignedMerkleRoot
        { merkleRoot, previousMerkleRoot, signatures, committeePubKeys }
    ) =
    Constr zero
      [ toData merkleRoot
      , toData previousMerkleRoot
      , toData signatures
      , toData committeePubKeys
      ]

-- | `SignedMerkleRootMint` parameterizes the onchain minting policy.
newtype SignedMerkleRootMint = SignedMerkleRootMint
  { -- | `sidechainParams` includes the `SidechainParams`
    sidechainParams ∷ SidechainParams
  , -- | `updateCommitteeHashCurrencySymbol` is the `CurrencySymbol` which
    -- (uniquely) identifies the utxo for which the `UpdateCommitteeHashDatum`
    -- resides.
    updateCommitteeHashCurrencySymbol ∷ CurrencySymbol
  }

derive instance Generic SignedMerkleRootMint _
derive instance Newtype SignedMerkleRootMint _
instance ToData SignedMerkleRootMint where
  toData
    (SignedMerkleRootMint { sidechainParams, updateCommitteeHashCurrencySymbol }) =
    Constr zero
      [ toData sidechainParams
      , toData updateCommitteeHashCurrencySymbol
      ]

-- | `SaveRootParams` is the offchain parameter for MPTRoot (`MPTRoot.saveRoot`)
-- | endpoint.
newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ ByteArray
  , previousMerkleRoot ∷ Maybe ByteArray
  , -- Public keys of all committee members and their corresponding signatures.
    committeeSignatures ∷ Array (SidechainPublicKey /\ Maybe SidechainSignature)
  }

-- | `MerkleRootInsertionMessage` is a data type for which committee members
-- | create signatures for (this corresponds to how signatures are verified onchain)
-- | ```
-- |  blake2b(cbor(MerkleRootInsertionMessage))
-- | ```
-- | See `MPTRoot.Utils.serialiseMrimHash`.
newtype MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { sidechainParams ∷ SidechainParams'
  , merkleRoot ∷ ByteArray
  , previousMerkleRoot ∷ Maybe ByteArray
  }

derive instance Generic MerkleRootInsertionMessage _
derive instance Newtype MerkleRootInsertionMessage _
instance ToData MerkleRootInsertionMessage where
  toData
    ( MerkleRootInsertionMessage
        { sidechainParams, merkleRoot, previousMerkleRoot }
    ) =
    Constr zero
      [ toData sidechainParams
      , toData merkleRoot
      , toData previousMerkleRoot
      ]
