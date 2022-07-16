module TrustlessSidechain.OffChain.DistributedSet where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Getter qualified as Getter
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Right)
import Control.Lens.Review qualified as Review
import Data.Default qualified as Default
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (Redeemer (Redeemer), TxOutRef)
import Ledger.Constraints as Constraints
import Ledger.Tx (
  ChainIndexTxOut,
 )
import Ledger.Tx qualified as Tx
import Plutus.ChainIndex (
  Page (nextPageQuery, pageItems),
  PageQuery (PageQuery, pageQueryLastItem, pageQuerySize),
 )
import Plutus.ChainIndex.Api (UtxosResponse (page))
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Error qualified as Error
import Plutus.Contract.Logging qualified as Logging
import Plutus.Contract.Request qualified as Request
import Plutus.V1.Ledger.Api (Datum (getDatum), MintingPolicy)
import Plutus.V1.Ledger.Value (AssetClass, TokenName (TokenName, unTokenName))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusPrelude qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Builtins.Class qualified as Class
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  DistributedSetParams (dspStr, dspTxOutRef),
 )
import TrustlessSidechain.OnChain.DistributedSet (
  DistributedSet (DistributedSet, dsSymbol),
  DistributedSetDatum,
  DistributedSetMint (DistributedSetMint, dsmTxOutRef),
  Node (Node, nodeDatum, nodeTokenName),
 )
import TrustlessSidechain.OnChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OnChain.Types (
  DistributedSetRedeemer (
    DistributedSetRedeemer,
    dsStr
  ),
 )
import Prelude qualified

rootTokenName :: TokenName
rootTokenName = ""

{- | 'foldUtxoRefsWithCurrency' Folds through each 'UtxosResponse's of utxos
 given with the given 'AssetClass' and 'PageQuery'.

 Loosely, this is the "currency-version" of 'Contract.foldUtxoRefsAt'
-}
foldUtxoRefsWithCurrency ::
  forall w s e a.
  AsContractError e =>
  (a -> UtxosResponse -> Contract w s e a) ->
  a ->
  PageQuery TxOutRef ->
  AssetClass ->
  Contract w s e a
foldUtxoRefsWithCurrency f z pq astCls =
  query pq >>= go z
  where
    -- This is directly modeled off of 'Contract.foldUtxoRefsAt' which is
    -- curiously a foldl and doesn't allow "short circuiting" if we find it
    -- early i.e., this always is the worst case search time complexity.
    --
    -- A rather curious choice by the Plutus designers! But we copy them
    -- anyways :^)
    query :: PageQuery TxOutRef -> Contract w s e UtxosResponse
    query = flip Contract.utxoRefsWithCurrency astCls

    go :: a -> UtxosResponse -> Contract w s e a
    go acc utxoRsp =
      f acc utxoRsp >>= \acc' ->
        case nextPageQuery (page utxoRsp) of
          Nothing -> return acc'
          Just pq' -> query pq' >>= go acc'

{- | 'foldUtxoRefsWithCurrency' Folds through each 'UtxosResponse's of utxos
 given with the given 'AssetClass' and 'PageQuery'.
-}
utxosWithCurrency ::
  forall w s e.
  AsContractError e =>
  PageQuery TxOutRef ->
  AssetClass ->
  Contract w s e (Map TxOutRef ChainIndexTxOut)
utxosWithCurrency = foldUtxoRefsWithCurrency go Map.empty
  where
    go acc utxoRsp =
      fmap (Prelude.mappend acc . Map.fromList . mapMaybe id) $
        traverse (\ref -> fmap (ref,) <$> Request.txOutFromRef ref)
        -- the 'fmap' here uses the Maybe instance.
        $
          pageItems $
            page utxoRsp

{- | 'findDistributedSetOutput' finds the transaction which we must insert to
 (if it exists).
-}
findDistributedSetOutput ::
  forall w s e.
  AsContractError e =>
  DistributedSet ->
  TokenName ->
  Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DistributedSetDatum, TokenName))
findDistributedSetOutput ds tn = do
  queryTn (Value.tokenName "") >>= \case
    Nothing -> return Nothing
    Just v -> fmap Just $ go v (0, lengthOfByteString str)
  where
    queryUtxos :: TokenName -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    queryUtxos = utxosWithCurrency pq . Value.assetClass (dsSymbol ds)

    queryTn :: TokenName -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DistributedSetDatum, TokenName))
    queryTn inp =
      Indexed.itoList <$> queryUtxos inp >>= \case
        [] -> return Nothing
        [(ref, o)]
          | Just (dat :: DistributedSetDatum) <- Class.fromBuiltinData . getDatum =<< Fold.preview (Tx.ciTxOutDatum . _Right) o
            , Getter.view Tx.ciTxOutAddress o == DistributedSet.insertAddress ds ->
            return $ Just (ref, o, dat, inp)
        _ ->
          Contract.throwError $
            Error._OtherContractError
              Review.# "DistributedSet internal error: there should be at most 1 node with the given token name"

    pq :: PageQuery TxOutRef
    pq = PageQuery {pageQuerySize = Default.def, pageQueryLastItem = Nothing}

    str :: BuiltinByteString
    str = unTokenName tn

    -- Binary search which searches for the largest TokenName (in size) for
    -- which the query returns 'Just' in the interval @(lo, hi]@.
    --
    -- Note this is possible because we have a trie data structure.
    go :: (TxOutRef, ChainIndexTxOut, DistributedSetDatum, TokenName) -> (Integer, Integer) -> Contract w s e (TxOutRef, ChainIndexTxOut, DistributedSetDatum, TokenName)
    go v (lo, hi)
      | lo + 1 == hi = fromMaybe v <$> queryTn (takeTokenName hi tn)
      | otherwise =
        queryTn (takeTokenName mid tn) >>= \case
          Just v' -> go v' (mid, hi)
          Nothing -> go v (lo, mid)
      where
        mid = lo + ((hi - lo) `Builtins.divideInteger` 2)

        takeTokenName :: Integer -> TokenName -> TokenName
        takeTokenName k = TokenName . takeByteString k . unTokenName

-- | 'distributedSetInsert' is the offchain code to build the distributed set
distributedSetInsert :: DistributedSetParams -> Contract () TrustlessSidechainSchema Text ()
distributedSetInsert dsp =
  findDistributedSetOutput ds (Value.tokenName $ Class.fromBuiltin str) >>= \case
    Nothing -> Contract.throwError "error: distributed set does not exist"
    Just (ref, o, dat, tn) ->
      let node :: Node
          node = Node {nodeTokenName = tn, nodeDatum = dat}
       in case DistributedSet.nodeNexts str node of
            Nothing -> Contract.throwError "error: inserting bad string"
            Just nodes -> do
              let lookups =
                    Constraints.unspentOutputs (Map.singleton ref o)
                      Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
                      Prelude.<> Constraints.mintingPolicy mp

                  redeemer :: Redeemer
                  redeemer = Redeemer $ Class.toBuiltinData $ DistributedSetRedeemer {dsStr = str}

                  tx = Prelude.mappend (Constraints.mustSpendScriptOutput ref redeemer) $
                    flip (Fold.foldMapOf Fold.folded) nodes $
                      \n -> Constraints.mustPayToTheScript (nodeDatum n) $ Value.assetClassValue (Value.assetClass (dsSymbol ds) $ nodeTokenName n) 1

              ledgerTx <- Contract.submitTxConstraintsWith @DistributedSet lookups tx
              PlutusPrelude.void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

              Logging.logInfo @Text "added to the distributed set"
  where
    -- Aliases:
    ds :: DistributedSet
    ds = DistributedSet {dsSymbol = DistributedSet.distributedSetCurSymbol dsm}

    dsm :: DistributedSetMint
    dsm = DistributedSetMint {dsmTxOutRef = dspTxOutRef dsp}

    mp :: MintingPolicy
    mp = DistributedSet.distributedSetPolicy dsm

    str :: BuiltinByteString
    str = dspStr dsp
