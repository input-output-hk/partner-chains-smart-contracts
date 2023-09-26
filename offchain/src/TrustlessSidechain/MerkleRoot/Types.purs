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
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Scripts (ValidatorHash)
import Contract.Value (CurrencySymbol)
import Control.Alternative (guard)
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSAggregateSignatures)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)

-- | `SignedMerkleRootRedeemer` is the redeemer for the update committee hash
-- | validator
-- | This corresponds to the onchain type.
newtype SignedMerkleRootRedeemer = SignedMerkleRootRedeemer
  { previousMerkleRoot ∷ Maybe RootHash
  }

derive newtype instance Eq SignedMerkleRootRedeemer

derive instance Generic SignedMerkleRootRedeemer _

derive instance Newtype SignedMerkleRootRedeemer _

instance Show SignedMerkleRootRedeemer where
  show = genericShow

instance ToData SignedMerkleRootRedeemer where
  toData (SignedMerkleRootRedeemer { previousMerkleRoot }) = toData
    previousMerkleRoot

instance FromData SignedMerkleRootRedeemer where
  fromData dat = do
    previousMerkleRoot ← fromData dat
    pure $ SignedMerkleRootRedeemer { previousMerkleRoot }

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

derive newtype instance Eq MerkleRootInsertionMessage

derive instance Generic MerkleRootInsertionMessage _

derive instance Newtype MerkleRootInsertionMessage _

instance Show MerkleRootInsertionMessage where
  show = genericShow

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

instance FromData MerkleRootInsertionMessage where
  fromData = case _ of
    Constr ix [ sp, mr, pmr ] → do
      guard (ix == BigNum.fromInt 0)
      sidechainParams ← fromData sp
      merkleRoot ← fromData mr
      previousMerkleRoot ← fromData pmr
      pure $ MerkleRootInsertionMessage
        { sidechainParams
        , merkleRoot
        , previousMerkleRoot
        }
    _ → Nothing
