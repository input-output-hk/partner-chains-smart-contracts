{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.Types where

import Ledger.Crypto (PubKey)
import Ledger.Typed.Scripts qualified as Script
import PlutusTx (makeIsDataIndexed)

import PlutusTx.Prelude (BuiltinByteString, Integer)
import TrustlessSidechain.MerkleTree (MerkleProof)

{- | 'MerkleTreeEntry' (abbr. mte and pl. mtes) is the data which are the elements in the merkle tree
 for the MPTRootToken.
-}
data MerkleTreeEntry = MerkleTreeEntry
  { -- | 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
    mteIndex :: !Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    mteAmount :: !Integer
  , -- | arbitrary length bytestring that represents decoded bech32 cardano
    -- address. See [here](https://cips.cardano.org/cips/cip19/) for more details
    -- of bech32
    mteRecipient :: !BuiltinByteString
  , -- | sidechain epoch for which merkle tree was created
    mteSidechainEpoch :: !Integer
  , -- | 'mteHash' will be removed later TODO! Currently, we have this here to
    -- help test the system.
    mteHash :: !BuiltinByteString
  }

makeIsDataIndexed ''MerkleTreeEntry [('MerkleTreeEntry, 0)]

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide !BuiltinByteString -- Recipient's sidechain address
  | -- | 'SideToMain' indicates that we wish to mint FUEL on the mainchain.
    -- So, this includes which transaction in the sidechain we are
    -- transferring over to the main chain (hence the 'MerkleTreeEntry'), and
    -- the proof tha this actually happened on the sidechain (hence the
    -- 'MerkleProof')
    SideToMain !MerkleTreeEntry !MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the mainchain.

makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance Script.ValidatorTypes FUELRedeemer

{- | The Redeemer that is passed to the on-chain validator to update the
 committee
-}
data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { -- | The current committee's signatures for the 'newCommitteeHash'
    committeeSignatures :: ![BuiltinByteString]
  , -- | 'committeePubKeys' is the current committee public keys
    committeePubKeys :: [PubKey]
  , -- | 'newCommitteeHash' is the hash of the new committee
    newCommitteeHash :: !BuiltinByteString
  }

makeIsDataIndexed ''UpdateCommitteeHashRedeemer [('UpdateCommitteeHashRedeemer, 0)]

data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot :: BuiltinByteString
  , signatures :: [BuiltinByteString]
  , threshold :: !Integer -- Natural: the number of committee pubkeys needed to sign off
  , committeePubKeys :: [PubKey] -- Public keys of all committee members
  }

makeIsDataIndexed ''SignedMerkleRoot [('SignedMerkleRoot, 0)]

instance Script.ValidatorTypes SignedMerkleRoot

{-

{- | The Redeemer that is passed to the on-chain validator to insert the
 following suggested string (provided it does not exist)
-}
newtype DsRedeemer = DsRedeemer
  { dsStr :: BuiltinByteString
  }

makeIsDataIndexed ''DsRedeemer [('DsRedeemer, 0)]
-}
