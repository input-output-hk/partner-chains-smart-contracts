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
import Data.Text qualified as Text
import Ledger (Redeemer (Redeemer), TxOutRef)
import Ledger.Address as Address
import Ledger.Constraints as Constraints
import Ledger.Scripts (Datum (Datum))
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (
  ChainIndexTxOut,
 )
import Ledger.Tx qualified as Tx
import Plutus.ChainIndex (
  Page (nextPageQuery, pageItems),
  PageQuery (PageQuery, pageQueryLastItem, pageQuerySize),
 )
import Plutus.ChainIndex.Api (IsUtxoResponse (isUtxo), UtxosResponse (page))
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Effects qualified as Effects
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
    query npq = utxoRefsWithCurrency npq astCls
    -- At this point, you're probably wondering why don't we just use:
    -- > Contract.utxoRefsWithCurrency q astCls
    -- afterall, the plutus people have already written this for us! Well,
    -- the version that the plutus people wrote up just doesn't work... not
    -- sure why but on the plutip tests, 'Contract.utxoRefsWithCurrency'
    -- always will throw an error no matter what because the @cir@ variable
    -- is always 'Effects.UtxoSetAtResponse'.

    utxoRefsWithCurrency pageq assetClass = do
      cir <-
        Request.pabReq
          (Effects.ChainIndexQueryReq $ Effects.UtxoSetWithCurrency pageq assetClass)
          Effects._ChainIndexQueryResp
      case cir of
        Effects.UtxoSetAtResponse r -> pure r
        Effects.UtxoSetWithCurrencyResponse r -> pure r
        r ->
          Contract.throwError $
            Review.review Contract._ChainIndexContractError ("UtxoSetWithCurrencyResponse", r)

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
    go :: Map TxOutRef ChainIndexTxOut -> UtxosResponse -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    go acc utxoRsp =
      flip (flip (Fold.foldlMOf Fold.folded) acc) (pageItems $ page utxoRsp) $
        -- Why do we need to use 'Contract.utxoRefMembership ref' to
        -- test if @ref@ is in the utxo set? Apparently (according to
        -- Plutip in the tests) sometimes the 'utxoRefsWithCurrency'
        -- doesn't actually return a utxo (i.e., it returns outputs
        -- that are already spent). This is weird.... very weird..
        \acc' ref ->
          Contract.utxoRefMembership ref >>= \p ->
            Request.txOutFromRef ref >>= \case
              Just o | isUtxo p -> return $ acc' `Prelude.mappend` Map.singleton ref o
              _ -> return acc'

-- | 'logDs' logs the entire distributed set
logDs :: AsContractError e => Ds -> Contract w s e ()
logDs ds = do
  Contract.logInfo @Text (Text.pack ("Logging the distributed set: " `Prelude.mappend` Prelude.show ds))
  Contract.utxosAt (DistributedSet.insertAddress ds) >>= \utxos ->
    Fold.forMOf_ Fold.folded utxos $ \utxo ->
      Contract.logInfo @Text $ Text.pack $ Prelude.show utxo
  Contract.logInfo @Text "Logging the distributed set END"

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
  Contract.logInfo @Text "Starting findDsOutput"
  queryTn (Value.tokenName "") >>= \case
    Nothing -> return Nothing
    Just v -> fmap Just $ go v
  where
    queryUtxos :: TokenName -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    queryUtxos = utxosWithCurrency pq . Value.assetClass (dsSymbol ds)

    queryTn :: TokenName -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DsDatum, TokenName))
    queryTn inp = do
      Indexed.itoList <$> queryUtxos inp >>= \case
        [] -> return Nothing
        [(ref, o)]
          | Just (dat :: DsDatum) <- Class.fromBuiltinData . getDatum =<< Fold.preview (Tx.ciTxOutDatum . _Right) o
            , Getter.view Tx.ciTxOutAddress o == DistributedSet.insertAddress ds ->
            return $ Just (ref, o, dat, inp)
        _errs ->
          Contract.throwError $
            Error._OtherContractError
              Review.# ( "Ds internal error: there should be at most 1 node with the given token name"
                       )

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
    nextNodeStrLength inp node =
      -- the following check (i.e., the precondition of
      -- 'DistributedSet.existsNextNode') is completely unnecessary for this
      -- use case here.
      -- > | nPrefix node == inp = Nothing@
      nBranches node >>= \(branchPrefix, _ds) ->
        lengthOfByteString (nPrefix node) + lengthOfByteString branchPrefix + 1
          <$ PlutusPrelude.guard (DistributedSet.existsNextNode inp node)

-- | converts a 'Node' to the correpsonding 'DsDatum'
nodeToDatum :: Node -> DsDatum
nodeToDatum node =
  DsDatum
    { dsLeaf = nLeaf node
    , dsBranches = nBranches node
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

-- We include this function to help with generating the 'DsParams' in the
-- test suite.
--
-- TODO: honestly, it might be a good idea to do this to mint the NFT for
-- the UpdateCommitteeHash endpoint as well (because last time I recall I
-- had some issues with the 'Address' data type and its Schema instance).

-- | 'dsInit' is the offchain code to build the distributed set
dsInit :: DsParams -> Contract () TrustlessSidechainSchema Text AssetClass
dsInit dsp = do
  Contract.txOutFromRef oref >>= \case
    Nothing -> Contract.throwError "error: bad distributed set init unspent transaction doesn't exist."
    Just citxout -> do
      let -- variables creating the token
          sm = DistributedSet.dsCurSymbol dsm
          tn = TokenName $ dspStr dsp
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

-- | 'dsInsert' is the offchain code to build the distributed set
dsInsert :: DsParams -> Contract () TrustlessSidechainSchema Text ()
dsInsert dsp = do
  Contract.logInfo @Text $ "CURRENCY SYMBOL"
  Contract.logInfo @Text $ Text.pack $ Prelude.show $ DistributedSet.dsCurSymbol dsm
  findDsOutput ds (Value.tokenName $ Class.fromBuiltin str) >>= \case
    Nothing -> Contract.throwError "error: distributed set does not exist"
    Just inp@(ref, o, dat, tn) ->
      let node :: Node
          node = DistributedSet.mkNode (unTokenName tn) dat
       in case DistributedSet.insertNode str node of
            Nothing -> Contract.throwError "error: inserting bad string"
            Just nodes -> do
              let lookups =
                    Constraints.unspentOutputs (Map.singleton ref o)
                      Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
                      Prelude.<> Constraints.otherScript (DistributedSet.insertValidator ds)
                      Prelude.<> Constraints.mintingPolicy mp

                  redeemer :: Redeemer
                  redeemer = Redeemer $ Class.toBuiltinData $ DsRedeemer {dsStr = str}

                  tx =
                    Prelude.mconcat
                      [ Constraints.mustSpendScriptOutput ref redeemer
                      , flip (Fold.foldMapOf Fold.folded) nodes $
                          \n ->
                            let nTn = TokenName $ nPrefix n
                                val = Value.singleton (DistributedSet.dsCurSymbol dsm) nTn 1
                             in if unTokenName nTn == nPrefix node
                                  then
                                    Constraints.mustPayToTheScript
                                      (nodeToDatum n)
                                      val
                                  else
                                    Constraints.mustPayToOtherScript
                                      (Scripts.validatorHash (DistributedSet.insertValidator ds))
                                      (Datum (Class.toBuiltinData (nodeToDatum n)))
                                      val
                                      Prelude.<> Constraints.mustMintValue val
                      ]

              Logging.logInfo @Text "Inserting the following node"
              Logging.logInfo @Text $ Text.pack $ Prelude.show node
              Logging.logInfo @Text $ Text.pack $ Prelude.show inp
              Logging.logInfo @Text "Which results in nodes"
              Logging.logInfo @Text $ Text.pack $ Prelude.show nodes

              ledgerTx <- Contract.submitTxConstraintsWith @Ds lookups tx
              PlutusPrelude.void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

              Logging.logInfo @Text "Transaction is as follows."
              Logging.logInfo @Text $ Text.pack $ Prelude.show ledgerTx
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
