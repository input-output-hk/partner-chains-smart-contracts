module TrustlessSidechain.OffChain.UpdateCommitteeHash where

import TrustlessSidechain.OffChain.Types (
  UpdateCommitteeHashParams,
 )
import TrustlessSidechain.OffChain.Types qualified as OffChainTypes
import TrustlessSidechain.OnChain.Types (
  UpdateCommitteeHashRedeemer (
    UpdateCommitteeHashRedeemer
  ),
 )

import TrustlessSidechain.OnChain.Types qualified as OnChainTypes

import TrustlessSidechain.OnChain.UpdateCommitteeHash (
  UpdateCommitteeHash (UpdateCommitteeHash, cToken),
  UpdateCommitteeHashDatum (UpdateCommitteeHashDatum, committeeHash),
  UpdatingCommitteeHash,
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash

import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Logging qualified as Logging
import Plutus.V1.Ledger.Api (Datum (getDatum))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass, unAssetClass))
import Plutus.V1.Ledger.Value qualified as Value

import Data.Map (Map)
import Data.Map qualified as Map

import Control.Lens.Fold qualified as Fold
import Control.Lens.Getter qualified as Getter
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Right)

import Ledger (Redeemer (Redeemer), TxOutRef)
import Ledger.Constraints as Constraints
import Ledger.Tx (
  ChainIndexTxOut,
  ciTxOutDatum,
  ciTxOutValue,
 )
import Ledger.Tx qualified as Tx

import PlutusPrelude (void)
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude

import Data.Text (Text)

import Prelude qualified

{- | 'findCommitteeHashOutput' searches through the utxos to find the output
 that corresponds to NFT which identifies the committee hash on the block
 chain with the datum.
-}
findCommitteeHashOutput ::
  AsContractError e =>
  UpdateCommitteeHash ->
  Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, UpdateCommitteeHashDatum))
findCommitteeHashOutput uch =
  fmap go $
    Contract.utxosAt $
      UpdateCommitteeHash.updateCommitteeHashAddress uch
  where
    go ::
      Map TxOutRef ChainIndexTxOut ->
      Maybe (TxOutRef, ChainIndexTxOut, UpdateCommitteeHashDatum)
    go utxos = do
      let f :: (TxOutRef, ChainIndexTxOut) -> Bool
          f (_, o) = Value.assetClassValueOf (Getter.view ciTxOutValue o) (cToken uch) == 1
      (oref, o) <- find f $ Indexed.itoList utxos
      -- N.B. on the on-chain code, we assert that the datum is actually
      -- contained in the output -- see Note [Committee hash in output datum]
      dat <-
        Class.fromBuiltinData
          =<< getDatum <$> Fold.preview (ciTxOutDatum . _Right) o
      return (oref, o, dat)

{- | 'updateCommitteeHash' is the endpoint to submit the transaction to update
 the committee hash
-}
updateCommitteeHash :: UpdateCommitteeHashParams -> Contract w s Text ()
updateCommitteeHash uchp =
  findCommitteeHashOutput uch >>= \case
    Just (oref, o, dat)
      | committeeHash dat == cCommitteeHash -> do
        let redeemer =
              Redeemer $
                Class.toBuiltinData $
                  UpdateCommitteeHashRedeemer
                    { OnChainTypes.signature = OffChainTypes.signature uchp
                    , OnChainTypes.committeePubKeys = OffChainTypes.committeePubKeys uchp
                    , OnChainTypes.newCommitteeHash = nCommitteeHash
                    }
            val = uncurry Value.singleton (unAssetClass $ cToken uch) 1
            lookups =
              Constraints.unspentOutputs (Map.singleton oref o)
                Prelude.<> Constraints.otherScript
                  (UpdateCommitteeHash.updateCommitteeHashValidator uch)
                Prelude.<> Constraints.typedValidatorLookups
                  (UpdateCommitteeHash.typedUpdateCommitteeHashValidator uch)

            tx =
              Constraints.mustSpendScriptOutput oref redeemer
                Prelude.<> Constraints.mustPayToTheScript ndat val
        ledgerTx <- Contract.submitTxConstraintsWith @UpdatingCommitteeHash lookups tx
        void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
        Logging.logInfo @Text "updated committee hash"
      | otherwise -> Contract.throwError "incorrect committee provided"
    Nothing -> Contract.throwError "update committee hash output not found"
  where
    uch :: UpdateCommitteeHash
    uch =
      UpdateCommitteeHash
        { cToken =
            AssetClass
              { unAssetClass = (OffChainTypes.symbol uchp, OffChainTypes.token uchp)
              }
        }

    -- new committee hash from the endpoint parameters
    nCommitteeHash :: BuiltinByteString
    nCommitteeHash = UpdateCommitteeHash.aggregateKeys $ OffChainTypes.newCommitteePubKeys uchp

    -- current committee hash from the endpoint parameters
    cCommitteeHash :: BuiltinByteString
    cCommitteeHash = UpdateCommitteeHash.aggregateKeys (OffChainTypes.committeePubKeys uchp)

    ndat :: UpdateCommitteeHashDatum
    ndat =
      UpdateCommitteeHashDatum
        { UpdateCommitteeHash.committeeHash = nCommitteeHash
        }
