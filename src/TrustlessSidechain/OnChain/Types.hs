{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.Types where

import Ledger.Typed.Scripts qualified as Script
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Prelude (BuiltinByteString)

import Ledger.Crypto (PubKey)

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide !BuiltinByteString -- Recipient address
  | SideToMain -- !MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the blockchain.

makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance Script.ValidatorTypes FUELRedeemer

{- | The Redeemer that is passed to the on-chain validator to update the
 committee
-}
data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { signature :: !BuiltinByteString
  , committeePubKeys :: [PubKey]
  , newCommitteeHash :: !BuiltinByteString
  }

makeIsDataIndexed ''UpdateCommitteeHashRedeemer [('UpdateCommitteeHashRedeemer, 0)]