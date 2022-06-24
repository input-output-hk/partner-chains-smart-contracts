{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Getter qualified as Getter
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Just)
import Control.Lens.Tuple (_1, _2, _3)
import Control.Monad (when)
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
  BurnParams (BurnParams, amount, recipient, sidechainParams, sidechainSig),
  MintParams (MintParams, amount, proof, recipient, sidechainParams),
  SidechainParams,
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))

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
burn BurnParams {amount, sidechainParams, recipient, sidechainSig} = do
  let policy = FUELMintingPolicy.mintingPolicy sidechainParams
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient sidechainSig)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    (Constraint.mustMintValueWithRedeemer redeemer value)

{- | 'mint' is the endpoint for claiming transactions broadcasted to the main
 chain (the main chain is this chain)

 The steps are as follows for constructing this transaction:

      (1) Find all utxos with an MPTRootToken. Recall that the utxos with the
      MPTRootToken have as token name the merkle root of unhandled
      transactions collected by the side chain  -- see
      'TrustlessSidechain.OffChain.MPTRootTokenMintingPolicy' for this
      endpoint

      (2) Find the particular utxo with an MPTRootToken which has a Merkle
      Root that corresponds to the proof that was given in the end point (and
      failing otherwise). This is done with 'sideChainLeafTransactionHash' to
      replicate how the side chain producers computed the leaf, and verifying
      if the MerkleProof provided in the endpoint is the given Merkle Root.

      N.B. this isn't necessarily unique? But we only want to mint these
      transactions once I suppose.. so technically, an adversary could try to
      submit these transactions with different roots to "double mint" FUEL but
      the distributed set should solve this problem [see step (3) on the
      distributed set]

      (3) Build the transaction by:

          - Providing a reference input to the transaction with the
          MPTRootToken. TODO: we can't do this yet -- we need Plutus V2.

          - Providing the transaction for the distributed set which provides
          proof of non=membership. TODO:

          - Obviously, we need to provide the minting policy as well.
-}
mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint MintParams {amount, sidechainParams, recipient, proof} = do
  -- (1)
  mptRootTkns <- findMPTRootToken sidechainParams

  -- (2)
  let leaf = sideChainLeafTransactionHash recipient amount
  case find (MT.memberMp leaf proof . Getter.view _3) mptRootTkns of
    Nothing -> Contract.throwError "no MPTRootToken with Merkle root found"
    Just (_oref, _o, _rh) -> do
      -- (3)
      -- This is still TODO
      let policy = FUELMintingPolicy.mintingPolicy sidechainParams
          value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
          redeemer = Redeemer $ toBuiltinData $ SideToMain proof
      when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
      Contract.submitTxConstraintsWith @FUELRedeemer
        (Constraint.mintingPolicy policy)
        ( Constraint.mustMintValueWithRedeemer redeemer value
            <> Constraint.mustPayToPubKey recipient value
        )
