{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Ledger (
  MintingPolicy,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting),
  TxInfo (TxInfo),
 )
import Ledger qualified
import Ledger.Typed.Scripts qualified as Script
import Ledger.Value qualified as Value
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Types (SidechainParams)
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

{- | 'mkMintingPolicy' verifies the following

  1. MPTRootToken with the name of the Merkle root of the transaction
  calculated from the proof) can be found in the MPTRootTokenValidator.

  TODO: it doesn't do this yet.

  2. chainId the minting policy id

  TODO: it doesn't do this yet.

  3. recipient and amount matches the actual tx body contents
-}
{-# INLINEABLE mkMintingPolicy #-}
mkMintingPolicy :: SidechainParams -> FUELRedeemer -> ScriptContext -> Bool
mkMintingPolicy
  _sc
  mode
  ScriptContext
    { scriptContextPurpose = Minting ownSymbol
    , scriptContextTxInfo = TxInfo {txInfoMint}
    } =
    case mode of
      MainToSide _ _ ->
        verifyTokenAmount $ traceIfFalse "Can't burn a positive amount" . (< 0)
      SideToMain _ ->
        verifyTokenAmount $ traceIfFalse "Can't mint a negative amount" . (> 0)
    where
      verifyTokenAmount verify =
        case Value.flattenValue txInfoMint of
          [(sym, name, amount)] ->
            verify amount
              && traceIfFalse "Token Symbol is incorrect" (sym == ownSymbol)
              && traceIfFalse "Token Name is incorrect" (name == ownTokenName)
          _ -> False
      ownTokenName = Value.TokenName "FUEL"
mkMintingPolicy _ _ _ = False

mintingPolicy :: SidechainParams -> MintingPolicy
mintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = Script.wrapMintingPolicy
