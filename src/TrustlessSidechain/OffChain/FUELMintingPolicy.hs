{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Monad (when)
import Data.Text (Text)
import Ledger (CardanoTx)
import Ledger.Constraints qualified as Constraint
import Ledger.Value qualified as Value
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.Script.Utils.V2.Scripts qualified as ScriptUtils
import Plutus.V2.Ledger.Api (Redeemer (Redeemer))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams, amount, recipient, sidechainParams, sidechainSig),
  MintParams (MintParams, amount, recipient, sidechainParams),
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient, sidechainSig} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (ScriptUtils.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient sidechainSig)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    (Constraint.mustMintValueWithRedeemer redeemer value)

mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint MintParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (ScriptUtils.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData SideToMain
  when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    ( Constraint.mustMintValueWithRedeemer redeemer value
        <> Constraint.mustPayToPubKey recipient value
    )
