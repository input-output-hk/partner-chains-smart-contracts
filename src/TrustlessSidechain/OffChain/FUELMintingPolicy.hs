{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Monad (when)
import Data.Text (Text)
import Ledger (Redeemer (Redeemer))
import Ledger qualified
import Ledger.Constraints qualified as Constraint
import Ledger.Typed.Scripts qualified as Script
import Ledger.Value qualified as Value
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx (ToData (toBuiltinData), makeIsDataIndexed)
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams, amount, recipient, sidechainParams),
  MintParams (MintParams, amount, recipient, sidechainParams),
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy

-- | The Redeemer that's to be passed to onchain policy, indicating its mode of usage.
data FUELRedeemer
  = MainToSide !BuiltinByteString -- Recipient address
  | SideToMain -- !MerkleProof

-- Recipient address is in FUELRedeemer just for reference on the mainchain,
-- it's actually useful (and verified) on the sidechain, so it needs to be
-- recorded in the blockchain.

makeIsDataIndexed ''FUELRedeemer [('MainToSide, 0), ('SideToMain, 1)]

instance Script.ValidatorTypes FUELRedeemer

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text ()
burn BurnParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  tx <-
    Contract.submitTxConstraintsWith @FUELRedeemer
      (Constraint.mintingPolicy policy)
      (Constraint.mustMintValueWithRedeemer redeemer value)
  Contract.awaitTxConfirmed $ Ledger.getCardanoTxId tx

mint :: MintParams -> Contract () TrustlessSidechainSchema Text ()
mint MintParams {amount, sidechainParams, recipient = _} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData SideToMain
  when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
  tx <-
    Contract.submitTxConstraintsWith @FUELRedeemer
      (Constraint.mintingPolicy policy)
      (Constraint.mustMintValueWithRedeemer redeemer value)
  Contract.awaitTxConfirmed $ Ledger.getCardanoTxId tx
