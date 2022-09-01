module TrustlessSidechain.OffChain.DistributedSet where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Right)
import Control.Lens.Review qualified as Review
import Data.Default qualified as Default
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (TxOutRef)
import Ledger.Address qualified as Address
import Ledger.Scripts (Datum (getDatum))
import Ledger.Tx (
  ChainIndexTxOut,
 )
import Ledger.Tx qualified as Tx
import Plutus.ChainIndex (
  PageQuery (PageQuery, pageQueryLastItem, pageQuerySize),
 )
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Error qualified as Error
import Plutus.V1.Ledger.Value (TokenName (unTokenName), Value (getValue))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Utils qualified as Utils
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (dsConf),
  DsConfDatum,
  DsDatum (DsDatum, dsNext),
  DsKeyMint (DsKeyMint, dskmConfCurrencySymbol, dskmValidatorHash),
  Ib,
  Node (nNext),
 )
import TrustlessSidechain.OnChain.DistributedSet qualified as DistributedSet

import Prelude qualified

-- | 'logDs' logs the entire distributed set
logDs :: AsContractError e => Ds -> Contract w s e ()
logDs ds = do
  Contract.logInfo @Text (Text.pack ("Logging the distributed set: " Prelude.<> Prelude.show ds))
  Contract.utxosAt (DistributedSet.insertAddress ds) >>= \utxos ->
    Fold.forMOf_ Fold.folded utxos $ \utxo ->
      Contract.logInfo @Text $ Text.pack $ Prelude.show utxo
  Contract.logInfo @Text "Logging the distributed set END"

-- | 'dsToDsKeyMint' is a helper function to convert 'Ds' into 'DsKeyMint'
dsToDsKeyMint :: Ds -> DsKeyMint
dsToDsKeyMint ds =
  DsKeyMint {dskmConfCurrencySymbol = dsConf ds, dskmValidatorHash = DistributedSet.insertValidatorHash ds}

{- | 'findDsOutput' finds the transaction which we must insert to
 (if it exists) for the distributed set. It returns
 the 'TxOutRef' of the output to spend, the chain index information, the datum
 at that utxo to spend, and the 'TokenName' of the key of the utxo we want to
 spend; and finally the new nodes to insert (after replacing the given node)

 N.B. this is linear in the size of the distributed set... one should maintain
 an efficient offchain index of the utxos, and set up the appropriate actions
 when the list gets updated by someone else.
-}
findDsOutput ::
  forall w s e.
  AsContractError e =>
  Ds ->
  TokenName ->
  Contract w s e (Maybe ((TxOutRef, ChainIndexTxOut, DsDatum, TokenName), Ib Node))
findDsOutput ds tn =
  let go [] = Nothing
      go ((ref, o) : ts)
        | Just (dat :: DsDatum) <- Class.fromBuiltinData . getDatum =<< Fold.preview (Tx.ciTxOutDatum . _Right) o
          , Just vs <- Fold.preview Tx.ciTxOutValue o
          , -- If it's more clear, the following check can be written as follows
            -- > , Just [(tn, 1)] <- AssocMap.lookup (DistributedSet.dsKeyCurrencySymbol $ dsToDsKeyMint ds) $ getValue vs
            -- by the onchain code.
            Just ((tn', _) : _) <-
              fmap AssocMap.toList $
                AssocMap.lookup
                  ( DistributedSet.dsKeyCurrencySymbol $
                      dsToDsKeyMint ds
                  )
                  $ getValue vs
          , Just nnodes <- DistributedSet.insertNode (unTokenName tn) $ DistributedSet.mkNode (unTokenName tn') dat =
          Just ((ref, o, dat, tn'), nnodes)
        | otherwise = go ts
   in go . Indexed.itoList <$> Contract.utxosAt (DistributedSet.insertAddress ds)

{- | 'findDsConfOutput' finds the utxo which holds the configuration of the
 distributed set.
-}
findDsConfOutput ::
  AsContractError e =>
  Ds ->
  Contract w s e (TxOutRef, ChainIndexTxOut, DsConfDatum)
findDsConfOutput ds =
  Utils.utxosWithCurrency pq (Value.assetClass (dsConf ds) DistributedSet.dsConfTokenName)
    >>= \utxos -> case Map.toList utxos of
      (oref, o) : _
        | Just (dat :: DsConfDatum) <- Class.fromBuiltinData . getDatum =<< Fold.preview (Tx.ciTxOutDatum . _Right) o ->
          return (oref, o, dat)
      _ ->
        Contract.throwError $
          Error._OtherContractError
            Review.# "Ds error: configuration not found"
  where
    pq :: PageQuery TxOutRef
    pq = PageQuery {pageQuerySize = Default.def, pageQueryLastItem = Nothing}

-- | Converts a 'Node' to the correpsonding 'DsDatum'
nodeToDatum :: Node -> DsDatum
nodeToDatum node =
  DsDatum
    { dsNext = nNext node
    }

{- | 'ownTxOutRef' finds a 'TxOutRef' corresponding to 'ownPaymentPubKeyHash'

 This is used in the test suite to make testing easier.
-}
ownTxOutRef :: Contract w s Text TxOutRef
ownTxOutRef = do
  h <- Contract.ownPaymentPubKeyHash
  let addr = Address.pubKeyHashAddress h Nothing
  fmap Indexed.itoList (Contract.utxosAt addr)
    >>= \case
      [] -> Contract.throwError "no UTxO found"
      utxo : _ -> return $ fst utxo
