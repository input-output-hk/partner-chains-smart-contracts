{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Getter qualified as Getter
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Just)
import Control.Lens.Tuple (_1, _2)
import Control.Monad (when)
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (CardanoTx, Redeemer (Redeemer))
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Constraints qualified as Constraint
import Ledger.Contexts qualified as Contexts
import Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Ledger.Scripts (MintingPolicy)
import Ledger.Tx (
  ChainIndexTxOut,
  TxOutRef,
  ciTxOutValue,
 )
import Ledger.Value qualified as Value
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Value (TokenName (unTokenName), Value (getValue))
import PlutusCore qualified
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.MerkleTree (RootHash (RootHash))
import TrustlessSidechain.MerkleTree qualified as MT
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams, amount, recipient, sidechainParams),
  MintParams (MintParams, amount, recipient, sidechainParams),
  SidechainParams,
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))
import Prelude qualified --(Semigroup(..))

{- | 'findMPTRootToken' searches through the utxos to find all outputs that
 corresponds to @MPTRootTokenValidator.address (sc :: SidechainParams)@.
-}
findMPTRootToken ::
  AsContractError e =>
  SidechainParams ->
  Contract w s e [(TxOutRef, ChainIndexTxOut, RootHash)]
findMPTRootToken sc =
  fmap (Fold.concatMapOf Fold.folded go . Indexed.itoList) $
    Contract.utxosAt $
      MPTRootTokenValidator.address sc
  where
    policy :: MintingPolicy
    policy = MPTRootTokenMintingPolicy.mintingPolicy sc

    -- This lens is sort of complicated:
    --  We focus on:
    --      1. the value
    --      2. the AssocMap of the value
    --      3. lookup the minting policy that correponds to the policy
    --      4. convert the map of TokenName to Integer into a list
    --      5. In 'MPTRootTokenMintingPolicy.mkMintingPolicy', we only only
    --      may mint 1 of these tokens, so we verify this as well [TODO: pretty
    --      sure this is optional, as an optimization we can remove this].
    --      6. We get the token name
    --      7. we "reshape" the token name into the desired output type.
    go ::
      (TxOutRef, ChainIndexTxOut) ->
      [(TxOutRef, ChainIndexTxOut, RootHash)]
    go (oref, o) =
      Fold.toListOf
        ( ciTxOutValue
            . Getter.to getValue
            . Getter.to
              (AssocMap.lookup (Contexts.scriptCurrencySymbol policy))
            . _Just
            . Getter.to AssocMap.toList
            . Fold.folded
            -- TODO: this 'filtered' step is optional.
            . Fold.filtered ((== 1) . Getter.view _2)
            . _1
            . Getter.to unTokenName
            . Getter.to RootHash
            . Getter.to (oref,o,)
        )
        o

{- | 'sideChainLeafTransactionHash' gives the hash of how the sidechain block
 producers compute the leaf of the Merkle tree of the unhandled transactions
 i.e., given a transaction @tx@ with recipient @txRecipient@ and amount
 @txAmount@, we compute
 > blake2(txRecipient, txAmount)
-}
sideChainLeafTransactionHash :: PaymentPubKeyHash -> Integer -> BuiltinByteString
sideChainLeafTransactionHash txRecipient txAmount =
  Builtins.blake2b_256 msg
  where
    msg =
      getPubKeyHash (unPaymentPubKeyHash txRecipient)
        `appendByteString` Class.stringToBuiltinByteString (PlutusCore.show txAmount)

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    (Constraint.mustMintValueWithRedeemer redeemer value)

mintWithUtxo :: Maybe (Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut) -> MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mintWithUtxo utxo MintParams {amount, sidechainParams, recipient} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData $ SideToMain MT.emptyMp -- we don't use the merkle tree right now, so we just fill this with the empty merkle proof
      lookups =
        Constraint.mintingPolicy policy
          Prelude.<> maybe Prelude.mempty Constraint.unspentOutputs utxo
      tx =
        ( Constraint.mustMintValueWithRedeemer redeemer value
            <> Constraint.mustPayToPubKey recipient value
        )
  when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
  Contract.submitTxConstraintsWith @FUELRedeemer lookups tx

mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint = mintWithUtxo Nothing
