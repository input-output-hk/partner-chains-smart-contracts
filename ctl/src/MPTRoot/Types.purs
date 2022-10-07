-- | 'MPTRoot.Types' contains types relating to the 'MPTRoot' endpoint.
--
-- Note: the reason for the existence of this module is because there are some
-- cyclic dependencies between 'MPTRoot' and 'UpdateCommitteeHash' without
-- this.
module MPTRoot.Types
  ( SignedMerkleRoot(SignedMerkleRoot)
  , SignedMerkleRootMint(SignedMerkleRootMint)
  , SaveRootParams(SaveRootParams)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class ToData
  , PlutusData(..)
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value (CurrencySymbol, TokenName)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)

-- | 'SignedMerkleRoot' is the redeemer for the minting policy.
data SignedMerkleRoot = SignedMerkleRoot
  { -- The new merkle root to insert.
    merkleRoot ∷ ByteArray
  , -- Either 'Just' the last merkle root (in the case it exists), or 'Nothing'
    -- if there is no such last merkle root (i.e., in the first transaction).
    lastMerkleRoot ∷ Maybe ByteArray
  , -- Ordered as their corresponding keys (also the same length as
    -- 'committeePubKeys')
    signatures ∷ Array Signature
  , -- Sorted public keys of all committee members
    committeePubKeys ∷ Array PubKey
  }

derive instance Generic SignedMerkleRoot _
instance ToData SignedMerkleRoot where
  toData
    ( SignedMerkleRoot
        { merkleRoot, lastMerkleRoot, signatures, committeePubKeys }
    ) =
    Constr zero
      [ toData merkleRoot
      , toData lastMerkleRoot
      , toData signatures
      , toData committeePubKeys
      ]

-- | 'SignedMerkleRootMint' parameterizes the onchain minting policy.
newtype SignedMerkleRootMint = SignedMerkleRootMint
  { -- | 'sidechainParams' includes the 'SidechainParams'
    sidechainParams ∷ SidechainParams
  , -- | 'updateCommitteeHashCurrencySymbol' is the 'CurrencySymbol' which
    -- (uniquely) identifies the utxo for which the 'UpdateCommitteeHashDatum'
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

-- | 'SaveRootParams' is the offchain parameter for MPTRoot ('MPTRoot.saveRoot')
-- endpoint.
newtype SaveRootParams = SaveRootParams
  { sidechainParams ∷ SidechainParams
  , merkleRoot ∷ ByteArray
  , lastMerkleRoot ∷ Maybe ByteArray
  , -- Public keys of all committee members and their corresponding signatures.
    committeeSignatures ∷ Array (PubKey /\ Maybe Signature)
  }
