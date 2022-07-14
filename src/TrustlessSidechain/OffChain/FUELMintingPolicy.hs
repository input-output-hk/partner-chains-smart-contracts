{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Monad (when)
import Data.Map qualified as Map
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
  BurnParams (BurnParams, amount, recipient, sidechainParams, sidechainSig),
  MintParams (MintParams, amount, recipient, sidechainParams),
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))
import Prelude qualified --(Semigroup(..))

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient, sidechainSig} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient sidechainSig)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    (Constraint.mustMintValueWithRedeemer redeemer value)

mintWithUtxo :: Maybe (Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut) -> MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mintWithUtxo utxo MintParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData SideToMain
      lookups = case utxo of
        Nothing -> Constraint.mintingPolicy policy
        Just u -> Constraint.mintingPolicy policy Prelude.<> Constraint.unspentOutputs u
      tx =
        ( Constraint.mustMintValueWithRedeemer redeemer value
            <> Constraint.mustPayToPubKey recipient value
        )
  when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
  Contract.submitTxConstraintsWith @FUELRedeemer lookups tx

mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint = mintWithUtxo Nothing
