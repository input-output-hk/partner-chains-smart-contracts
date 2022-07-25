{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.Types where

import Ledger.Crypto (PubKey)
import Ledger.Typed.Scripts qualified as Script
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Prelude (BuiltinByteString, Integer)

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide !BuiltinByteString -- Recipient's sidechain address
  | SideToMain -- !MerkleProof

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
