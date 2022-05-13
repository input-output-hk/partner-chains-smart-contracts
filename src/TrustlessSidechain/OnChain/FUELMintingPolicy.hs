{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OnChain.FUELMintingPolicy where

import Control.Monad (when)
import Data.Text (Text)

import Ledger (
  MintingPolicy,
  Redeemer (Redeemer),
  ScriptContext (..),
  ScriptPurpose (..),
  TxInfo (..),
 )
import Ledger qualified
import Ledger.Constraints qualified as Constraint
import Ledger.Typed.Scripts qualified as Script
import Ledger.Value qualified as Value

import Plutus.Contract (
  Contract,
  Endpoint,
  type (.\/),
 )
import Plutus.Contract qualified as Contract

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

instance Script.ValidatorTypes FUELRedeemer

{-# INLINEABLE mkFUELMintingPolicy #-}
mkFUELMintingPolicy :: SidechainParams -> FUELRedeemer -> ScriptContext -> Bool
mkFUELMintingPolicy
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
mkFUELMintingPolicy _ SideToMain ScriptContext {scriptContextPurpose = Minting _} = True
mkFUELMintingPolicy _ _ _ = False

fuelMintingPolicy :: SidechainParams -> MintingPolicy
fuelMintingPolicy param =
  Ledger.mkMintingPolicyScript
    ($$(compile [||wrap . mkFUELMintingPolicy||]) `applyCode` liftCode param)
  where
    wrap = Script.wrapMintingPolicy

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
burn BurnParams {amount, sidechainParams, recipient} = do
  let policy = fuelMintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  tx <-
    Contract.submitTxConstraintsWith @FUELRedeemer
      (Constraint.mintingPolicy policy)
      (Constraint.mustMintValueWithRedeemer redeemer value)
  Contract.awaitTxConfirmed $ Ledger.getCardanoTxId tx

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
mint MintParams {amount, sidechainParams, recipient = _} = do
  let policy = fuelMintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData SideToMain
  when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
  tx <-
    Contract.submitTxConstraintsWith @FUELRedeemer
      (Constraint.mintingPolicy policy)
      (Constraint.mustMintValueWithRedeemer redeemer value)
  Contract.awaitTxConfirmed $ Ledger.getCardanoTxId tx
