-- | `MerkleRoot.Types` contains types relating to the `MerkleRoot` endpoint.
-- |
-- |  Note: the reason for the existence of this module is because without this
-- | module, there are some cyclic dependencies between `MerkleRoot` and
-- | `UpdateCommitteeHash`.
module TrustlessSidechain.MerkleRoot.Types
  ( SaveRootParams(SaveRootParams)
  , MerkleRootInsertionMessage(MerkleRootInsertionMessage)
  , SignedMerkleRootRedeemer(SignedMerkleRootRedeemer)
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSAggregateSignatures)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)

-- | `SignedMerkleRootRedeemer` is the redeemer for the update committee hash
-- | validator
-- | This corresponds to the onchain type.
newtype SignedMerkleRootRedeemer = SignedMerkleRootRedeemer
  { previousMerkleRoot ∷ Maybe RootHash
  }

derive instance Generic SignedMerkleRootRedeemer _

derive instance Newtype SignedMerkleRootRedeemer _

instance ToData SignedMerkleRootRedeemer where
  toData (SignedMerkleRootRedeemer { previousMerkleRoot }) = toData
    previousMerkleRoot

-- | `SaveRootParams` is the offchain parameter for MerkleRoot (`MerkleRoot.saveRoot`)
-- | endpoint.
newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ RootHash
  , previousMerkleRoot ∷ Maybe RootHash
  , aggregateSignature ∷ ATMSAggregateSignatures
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
