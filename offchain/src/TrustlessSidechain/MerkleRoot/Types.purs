-- | `MerkleRoot.Types` contains types relating to the `MerkleRoot` endpoint.
-- |
-- |  Note: the reason for the existence of this module is because without this
-- | module, there are some cyclic dependencies between `MerkleRoot` and
-- | `UpdateCommitteeHash`.
module TrustlessSidechain.MerkleRoot.Types
  ( SignedMerkleRootMint(SignedMerkleRootMint)
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
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSAggregateSignatures)
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)

-- | `SignedMerkleRootMint` parameterizes the onchain minting policy.
newtype SignedMerkleRootMint = SignedMerkleRootMint
  { -- | `sidechainParams` includes the `SidechainParams`
    sidechainParams ∷ SidechainParams
  , -- | `committeeCertificateVerificationCurrencySymbol` is the `CurrencySymbol` which
    -- (uniquely) identifies the utxo for which the `UpdateCommitteeDatum`
    -- resides.
    committeeCertificateVerificationCurrencySymbol ∷ CurrencySymbol
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
        , committeeCertificateVerificationCurrencySymbol
        , merkleRootValidatorHash
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData sidechainParams
      , toData committeeCertificateVerificationCurrencySymbol
      , toData merkleRootValidatorHash
      ]

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
