{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Ledger (MintingPolicy, ScriptContext, mkMintingPolicyScript)
import Ledger.Typed.Scripts (wrapMintingPolicy)

import PlutusTx
import PlutusTx.Prelude

import TrustlessSidechain.OnChain.CommitteeCandidateValidator (SidechainParams)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: SidechainParams -> () -> ScriptContext -> Bool
mkPolicy _ _ _ = True

fuelMP :: SidechainParams -> MintingPolicy
fuelMP param =
  mkMintingPolicyScript
    ($$(compile [||wrapMintingPolicy . mkPolicy||]) `applyCode` liftCode param)
