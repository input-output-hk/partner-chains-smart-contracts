{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Ledger (
  MintingPolicy,
  ScriptContext (..),
  ScriptPurpose (..),
  TxInfo (..),
 )
import Ledger qualified
import Ledger.Typed.Scripts qualified as Script
import Ledger.Value qualified as Value
import PlutusTx (applyCode, compile, liftCode, makeIsDataIndexed)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide !BuiltinByteString -- Recipient address
  | SideToMain -- !MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the blockchain.

makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance Script.ValidatorTypes FUELRedeemer

{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> FUELRedeemer -> ScriptContext -> Bool
mkMintingPolicy
  _
  (MainToSide _)
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint}
    } =
    case Value.flattenValue txInfoMint of
      [(sym, name, amount)] ->
        traceIfFalse "Can't burn a positive amount" (amount < 0)
          && traceIfFalse "Token Symbol is incorrect" (sym == ownSymbol)
          && traceIfFalse "Token Name is incorrect" (name == ownTokenName)
      _ -> False
    where
      ownTokenName = Value.TokenName "FUEL"
mkMintingPolicy _ SideToMain ScriptContext {scriptContextPurpose = Minting _} = True
mkMintingPolicy _ _ _ = False

mintingPolicy :: SidechainParams -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = Script.wrapMintingPolicy
