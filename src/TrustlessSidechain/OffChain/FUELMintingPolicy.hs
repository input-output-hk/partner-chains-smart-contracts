{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Monad (when)
import Data.Text (Text)
import Ledger (CardanoTx, Redeemer (Redeemer))
import Ledger qualified
import Ledger.Constraints qualified as Constraint
import Ledger.Value qualified as Value
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (..),
  MintParams (..),
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    (Constraint.mustMintValueWithRedeemer redeemer value)

mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint MintParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData SideToMain
  when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    ( Constraint.mustMintValueWithRedeemer redeemer value
        <> Constraint.mustPayToPubKey recipient value
    )
