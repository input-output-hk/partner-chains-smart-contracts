{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.Types where

import Ledger (PubKey)
import Ledger.Typed.Scripts qualified as Script
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Prelude (BuiltinByteString)

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide
      !BuiltinByteString -- Recipient's sidechain address
      !BuiltinByteString -- Recipient's sidechain signature
  | SideToMain -- !MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the mainchain. Signature is added to make sure the address does
-- not refer to a script.

makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance Script.ValidatorTypes FUELRedeemer

data SignedMerkleRoot = SignedMerkleRoot
  { merkleRoot :: BuiltinByteString
  , signature :: BuiltinByteString
  , committeePubKeys :: [PubKey] -- Public keys of all committee members
  }

makeIsDataIndexed ''SignedMerkleRoot [('SignedMerkleRoot, 0)]

instance Script.ValidatorTypes SignedMerkleRoot
