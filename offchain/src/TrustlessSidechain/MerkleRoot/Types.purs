-- | `MerkleRoot.Types` contains types relating to the `MerkleRoot` endpoint.
-- |
-- |  Note: the reason for the existence of this module is because without this
-- | module, there are some cyclic dependencies between `MerkleRoot` and
-- | `UpdateCommitteeHash`.
module TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  , SaveRootParams(SaveRootParams)
  , MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , toData
  )
import Contract.Scripts (ValidatorHash)
import Contract.Value (CurrencySymbol)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

-- | `SignedMerkleRoot` is the redeemer for the minting policy.
data SignedMerkleRoot = SignedMerkleRoot
  { -- The new merkle root to insert.
    merkleRoot ∷ RootHash
  , -- Either `Just` the last merkle root (in the case it exists), or `Nothing`
    -- if there is no such last merkle root (i.e., in the first transaction).
    previousMerkleRoot ∷ Maybe RootHash
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
    Constr (BigNum.fromInt 0)
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
  , -- | `merkleRootValidatorHash` is used to ensure that the token is paid to
    -- the "right" script
    merkleRootValidatorHash ∷ ValidatorHash
  }

derive instance Generic SignedMerkleRootMint _

derive instance Newtype SignedMerkleRootMint _

instance ToData SignedMerkleRootMint where
  toData
    ( SignedMerkleRootMint
        { sidechainParams
        , updateCommitteeHashCurrencySymbol
        , merkleRootValidatorHash
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData sidechainParams
      , toData updateCommitteeHashCurrencySymbol
      , toData merkleRootValidatorHash
      ]

-- | `SaveRootParams` is the offchain parameter for MerkleRoot (`MerkleRoot.saveRoot`)
-- | endpoint.
newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ RootHash
  , previousMerkleRoot ∷ Maybe RootHash
  , -- Public keys of all committee members and their corresponding signatures.
    committeeSignatures ∷ Array (SidechainPublicKey /\ Maybe SidechainSignature)
  }

-- | `MerkleRootInsertionMessage` is a data type for which committee members
-- | create signatures for (this corresponds to how signatures are verified onchain)
-- | ```
-- |  blake2b(cbor(MerkleRootInsertionMessage))
-- | ```
-- | See `MerkleRoot.Utils.serialiseMrimHash`.
newtype MerkleRootInsertionMessage = MerkleRootInsertionMessage
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ RootHash
  , previousMerkleRoot ∷ Maybe RootHash
  }

derive instance Generic MerkleRootInsertionMessage _

derive instance Newtype MerkleRootInsertionMessage _

instance ToData MerkleRootInsertionMessage where
  toData
    ( MerkleRootInsertionMessage
        { sidechainParams, merkleRoot, previousMerkleRoot }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData sidechainParams
      , toData merkleRoot
      , toData previousMerkleRoot
      ]
