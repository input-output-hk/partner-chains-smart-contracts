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
import Ledger.Scripts qualified as Scripts
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
import PlutusTx.Builtins.Class qualified as Class
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  DsParams (dspStr, dspTxOutRef),
 )
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (Ds, dsSymbol),
  DsDatum (DsDatum, dsBranches, dsLeaf),
  DsMint (DsMint, dsmTxOutRef),
  Node (nBranches, nLeaf, nPrefix),
 )
import TrustlessSidechain.OnChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OnChain.Types (
  DsRedeemer (
    DsRedeemer,
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

{- | 'findDsOutput' finds the transaction which we must insert to
 (if it exists).
-}
findDsOutput ::
  forall w s e.
  AsContractError e =>
  Ds ->
  TokenName ->
  Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DsDatum, TokenName))
findDsOutput ds tn = do
  queryTn (Value.tokenName "") >>= \case
    Nothing -> return Nothing
    Just v -> fmap Just $ go v
  where
    queryUtxos :: TokenName -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    queryUtxos = utxosWithCurrency pq . Value.assetClass (dsSymbol ds)

    queryTn :: TokenName -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DsDatum, TokenName))
    queryTn inp =
      Indexed.itoList <$> queryUtxos inp >>= \case
        [] -> return Nothing
        [(ref, o)]
          | Just (dat :: DsDatum) <- Class.fromBuiltinData . getDatum =<< Fold.preview (Tx.ciTxOutDatum . _Right) o
            , Getter.view Tx.ciTxOutAddress o == DistributedSet.insertAddress ds ->
            return $ Just (ref, o, dat, inp)
        _ ->
          Contract.throwError $
            Error._OtherContractError
              Review.# "Ds internal error: there should be at most 1 node with the given token name"

    pq :: PageQuery TxOutRef
    pq = PageQuery {pageQuerySize = Default.def, pageQueryLastItem = Nothing}

    str :: BuiltinByteString
    str = unTokenName tn

    go ::
      (TxOutRef, ChainIndexTxOut, DsDatum, TokenName) ->
      Contract w s e (TxOutRef, ChainIndexTxOut, DsDatum, TokenName)
    go inp@(_ref, _o, dat, t) =
      let n = DistributedSet.mkNode (unTokenName t) dat
       in PlutusPrelude.join
            <$> traverse
              (queryTn . TokenName . flip takeByteString str)
              (nextNodeStrLength str n)
            >>= \case
              Nothing -> return inp
              Just inp' -> go inp'

    -- finds the length of the next prefix to search for (provided it exists)
    nextNodeStrLength :: BuiltinByteString -> Node -> Maybe Integer
    nextNodeStrLength inp node
      -- this is completely unnecessary for this use case here.
      -- @| nPrefix node == inp = Nothing@
      | otherwise =
        nBranches node >>= \(branchPrefix, _ds) ->
          lengthOfByteString (nPrefix node) + lengthOfByteString branchPrefix + 1
            <$ PlutusPrelude.guard (DistributedSet.existsNextNode inp node)
      -- N.B. the precondition of
      -- 'DistributedSet.existsNextNode' is automatically
      -- satisfied here.
      | otherwise = Nothing

-- | converts a 'Node' to the correpsonding 'DsDatum'
nodeToDatum :: Node -> DsDatum
nodeToDatum node =
  DsDatum
    { dsLeaf = nLeaf node
    , dsBranches = nBranches node
    }

-- | 'dsInit' is the offchain code to build the distributed set
dsInit :: DsParams -> Contract () TrustlessSidechainSchema Text AssetClass
dsInit dsp = do
  Contract.txOutFromRef oref >>= \case
    Nothing -> Contract.throwError "error: bad distributed set init unspent transaction doesn't exist."
    Just citxout -> do
      let -- variables creating the token
          sm = DistributedSet.dsCurSymbol dsm
          tn = TokenName ""
          val = Value.singleton sm tn 1
          ast = Value.assetClass sm tn

          dat =
            DsDatum
              { dsLeaf = False
              , dsBranches = Nothing
              }

          -- lookups required to build the transaction
          lookups =
            Constraints.unspentOutputs (Map.singleton oref citxout)
              Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
              Prelude.<> Constraints.mintingPolicy mp

          -- the transaction
          tx =
            Constraints.mustSpendPubKeyOutput oref
              Prelude.<> Constraints.mustMintValue val
              Prelude.<> Constraints.mustPayToTheScript dat val

      ledgerTx <- Contract.submitTxConstraintsWith @Ds lookups tx

      PlutusPrelude.void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

      Contract.logInfo @Text "Initialized distributed set"

      return ast
  where
    -- Aliases:
    oref :: TxOutRef
    oref = dspTxOutRef dsp

    ds :: Ds
    ds = Ds {dsSymbol = DistributedSet.dsCurSymbol dsm}

    dsm :: DsMint
    dsm = DsMint {dsmTxOutRef = dspTxOutRef dsp}

    mp :: MintingPolicy
    mp = DistributedSet.dsPolicy dsm

{-
data DsParams = DistributedSetParams
  { -- | The 'TxOutRef' which is used as the "genesis" transaction to create
    -- the distributed set.
    dspTxOutRef :: TxOutRef
  , -- | The 'BuiltinByteString' to insert
    dspStr :: BuiltinByteString
  }
  -}

-- | 'dsInsert' is the offchain code to build the distributed set
dsInsert :: DsParams -> Contract () TrustlessSidechainSchema Text ()
dsInsert dsp =
  findDsOutput ds (Value.tokenName $ Class.fromBuiltin str) >>= \case
    Nothing -> Contract.throwError "error: distributed set does not exist"
    Just (ref, o, dat, tn) ->
      let node :: Node
          node = DistributedSet.mkNode (unTokenName tn) dat
       in case DistributedSet.insertNode str node of
            Nothing -> Contract.throwError "error: inserting bad string"
            Just nodes -> do
              let lookups =
                    Constraints.unspentOutputs (Map.singleton ref o)
                      Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
                      Prelude.<> Constraints.mintingPolicy mp

                  redeemer :: Redeemer
                  redeemer = Redeemer $ Class.toBuiltinData $ DsRedeemer {dsStr = str}

                  tx =
                    Prelude.mconcat
                      [ Constraints.mustSpendScriptOutput ref redeemer
                      , flip (Fold.foldMapOf Fold.folded) nodes $
                          \n ->
                            let nTn = TokenName $ nPrefix n
                                scriptNode =
                                  Constraints.mustPayToTheScript
                                    (nodeToDatum n)
                                    (Value.assetClassValue (Value.assetClass (dsSymbol ds) $ nTn) 1)
                                mintTn =
                                  Constraints.mustMintCurrency
                                    (Scripts.mintingPolicyHash (DistributedSet.dsPolicy dsm))
                                    nTn
                                    1
                             in scriptNode Prelude.<> mintTn
                      ]

              ledgerTx <- Contract.submitTxConstraintsWith @Ds lookups tx
              PlutusPrelude.void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

              Logging.logInfo @Text "added to the distributed set"
  where
    -- Aliases:
    ds :: Ds
    ds = Ds {dsSymbol = DistributedSet.dsCurSymbol dsm}

    dsm :: DsMint
    dsm = DsMint {dsmTxOutRef = dspTxOutRef dsp}

    mp :: MintingPolicy
    mp = DistributedSet.dsPolicy dsm

    str :: BuiltinByteString
    str = dspStr dsp
