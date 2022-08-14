{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.FUELMintingPolicy where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Indexed qualified as Indexed
import Control.Monad (when)
import Data.Default qualified as Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (CardanoTx, Redeemer (Redeemer))
import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Constraints qualified as Constraint
import Ledger.Tx (
  ChainIndexTxOut,
  TxOutRef,
  ciTxOutAddress,
 )
import Ledger.Value qualified as Value
import Plutus.ChainIndex (
  PageQuery (PageQuery, pageQueryLastItem, pageQuerySize),
 )
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Plutus.V1.Ledger.Scripts qualified as Scripts
import Plutus.V1.Ledger.Value (TokenName (TokenName))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude
import TrustlessSidechain.MerkleTree (RootHash (unRootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams, amount, recipient, sidechainParams),
  MintParams (MintParams, amount, index, merkleProof, recipient, sidechainEpoch, sidechainParams),
  SidechainParams,
 )
import TrustlessSidechain.OffChain.Utils qualified as Utils
import TrustlessSidechain.OnChain.FUELMintingPolicy (FUELMint (FUELMint, fmMptRootTokenCurrencySymbol, fmSidechainParams))
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified as MPTRootTokenValidator
import TrustlessSidechain.OnChain.Types (
  FUELRedeemer (MainToSide, SideToMain),
  MerkleTreeEntry (MerkleTreeEntry, mteAmount, mteIndex, mteRecipient, mteSidechainEpoch),
 )

import Prelude qualified

{- | 'findMPTRootToken sc mp me' constructs the merkle root from the merkle
 proof @mp@ and the merkle entry @me@; and looks for the utxo with this
 MPTRootToken which corresponds to the given sidechain parameters @sc@ which is
 paid to an MPTRootTokenValidator.
-}
findMPTRootToken ::
  AsContractError e =>
  SidechainParams ->
  RootHash ->
  Contract w s e [(TxOutRef, ChainIndexTxOut)]
findMPTRootToken sc rh =
  filter go . Indexed.itoList <$> Utils.utxosWithCurrency pq assetClass
  where
    pq = PageQuery {pageQuerySize = Default.def, pageQueryLastItem = Nothing}

    assetClass = Value.assetClass (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sc) $ TokenName $ unRootHash rh

    -- TODO: as an optimization, I don't think testing if it is paid to the
    -- MPTRootTokenValidator is really necessary since the existence of the
    -- token is enough to imply that the sidechain has initiated this
    -- transaction.
    go :: (TxOutRef, ChainIndexTxOut) -> Bool
    go (_oref, o) = maybe False (== MPTRootTokenValidator.address sc) $ o Fold.^? ciTxOutAddress

burn :: BurnParams -> Contract () TrustlessSidechainSchema Text CardanoTx
burn BurnParams {amount, sidechainParams, recipient} = do
  let fm =
        FUELMint
          { fmSidechainParams = sidechainParams
          , fmMptRootTokenCurrencySymbol = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams
          }
      policy = FUELMintingPolicy.mintingPolicy fm
      value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
      redeemer = Redeemer $ toBuiltinData (MainToSide recipient)
  when (amount > 0) $ Contract.throwError "Can't burn a positive amount"
  Contract.submitTxConstraintsWith @FUELRedeemer
    (Constraint.mintingPolicy policy)
    (Constraint.mustMintValueWithRedeemer redeemer value)

{- | 'mintWithUtxo' does the following

      1. Locates the utxo with the MPTRootToken, which proves that the
      transaction happened on the sidechain.

      2. Build the transaction by

          * Providing a reference input to the transaction with the
          MPTRootToken

          TODO: it doesn't do this: we need PlutusV2 for reference inputs.
          For now, we consume and force it pay the value back (this isn't
          checked on chain since we will remove this feature later)

          * Calling the FUELMintingPolicy as usual.
-}
mintWithUtxo :: Maybe (Map Ledger.TxOutRef Ledger.ChainIndexTxOut) -> MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mintWithUtxo utxo MintParams {amount, index, sidechainEpoch, sidechainParams, recipient, merkleProof} =
  let mte =
        MerkleTreeEntry
          { mteIndex = index
          , mteAmount = amount
          , mteRecipient = getPubKeyHash $ unPaymentPubKeyHash recipient
          , mteSidechainEpoch = sidechainEpoch
          }
      cborMte = MPTRootTokenMintingPolicy.serialiseMte mte
      root = MerkleTree.rootMp cborMte merkleProof
   in findMPTRootToken sidechainParams root >>= \case
        [] -> Contract.throwError "error FUELMintingPolicy: no UTxO found"
        mptUtxo : _ -> do
          let fm =
                FUELMint
                  { fmSidechainParams = sidechainParams
                  , fmMptRootTokenCurrencySymbol = MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams
                  }
              policy = FUELMintingPolicy.mintingPolicy fm
              value = Value.singleton (Ledger.scriptCurrencySymbol policy) "FUEL" amount
              redeemer = Redeemer $ toBuiltinData $ SideToMain mte merkleProof

              -- TODO: the following line should be removed with reference inputs:
              mptRootTokenValidator = MPTRootTokenValidator.validator sidechainParams
              -- TODO: the following line should be removed with reference inputs:
              mptRootTokenValue =
                Value.singleton
                  (MPTRootTokenMintingPolicy.mintingPolicyCurrencySymbol sidechainParams)
                  (TokenName $ unRootHash root)
                  1

              lookups =
                Constraint.mintingPolicy policy
                  Prelude.<> Constraint.unspentOutputs (fromMaybe Prelude.mempty utxo Prelude.<> uncurry Map.singleton mptUtxo)
                  -- TODO: the following line should be removed with reference inputs:
                  Prelude.<> Constraint.otherScript mptRootTokenValidator
              tx =
                Constraint.mustMintValueWithRedeemer redeemer value
                  Prelude.<> Constraint.mustPayToPubKey recipient value
                  Prelude.<> Constraint.mustSpendScriptOutput (fst mptUtxo) Scripts.unitRedeemer
                  -- TODO: the following line should be removed with reference inputs:
                  Prelude.<> Constraint.mustPayToOtherScript (MPTRootTokenValidator.hash sidechainParams) Scripts.unitDatum mptRootTokenValue

          when (amount < 0) $ Contract.throwError "Can't mint a negative amount"
          Contract.submitTxConstraintsWith @FUELRedeemer lookups tx

mint :: MintParams -> Contract () TrustlessSidechainSchema Text CardanoTx
mint = mintWithUtxo Nothing
