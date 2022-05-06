{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Data.Text (Text)

import Ledger (
  MintingPolicy,
  ScriptContext,
  mkMintingPolicyScript,
 )
import Ledger.Typed.Scripts (wrapMintingPolicy)

import Plutus.Contract (
  Contract,
  Endpoint,
  type (.\/),
 )

import PlutusTx
import PlutusTx.Prelude

import TrustlessSidechain.OnChain.CommitteeCandidateValidator (SidechainParams)

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide !BuiltinByteString -- Recipient address
  | SideToMain -- !MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the blockchain.

makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

{-# INLINEABLE mkFUELMintingPolicy #-}
mkFUELMintingPolicy :: SidechainParams -> FUELRedeemer -> ScriptContext -> Bool
mkFUELMintingPolicy _ (MainToSide _) _ = True
mkFUELMintingPolicy _ SideToMain _ = True

fuelMintingPolicy :: SidechainParams -> MintingPolicy
fuelMintingPolicy param =
  mkMintingPolicyScript
    ($$(compile [||wrap . mkFUELMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = wrapMintingPolicy

type FUELMintingPolicySchema =
  Endpoint "burn" BurnParams .\/ Endpoint "mint" MintParams

data BurnParams = BurnParams
  { -- | Burnt amount in FUEL (Negative)
    amount :: Integer 
  , -- | SideChain address
    recipient :: BuiltinByteString
  , -- | passed for parametrization
    sidechainParams :: SidechainParams
  }

burn :: BurnParams -> Contract () FUELMintingPolicySchema Text ()
burn _ = pure ()

data MintParams = MintParams
  { -- | Minted amount in FUEL (Positive)
    amount :: Integer
  , -- | MainChain address
    recipient :: BuiltinByteString
  , -- | passed for parametrization
    sidechainParams :: SidechainParams
    -- , proof :: MerkleProof
  }

mint :: MintParams -> Contract () FUELMintingPolicySchema Text ()
mint _ = pure ()
