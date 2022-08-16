module TrustlessSidechain.OffChain.DistributedSet where

import Control.Lens.At qualified as At
import Control.Lens.Fold qualified as Fold
import Control.Lens.Getter qualified as Getter
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Right)
import Control.Lens.Prism qualified as Prism
import Control.Lens.Review qualified as Review
import Data.Default qualified as Default
import Data.List qualified as PreludeList
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (Redeemer (Redeemer), TxOutRef)
import Ledger.Address qualified as Address
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (Datum (Datum))
import Ledger.Scripts qualified as Scripts
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
import TrustlessSidechain.OffChain.Utils qualified as Utils
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (Ds, dsTxOutRef),
  DsDatum (DsDatum, dsEdge),
  DsMint (DsMint, dsmTxOutRef, dsmValidatorHash),
  Node (nEdge, nPrefix),
 )
import TrustlessSidechain.OnChain.DistributedSet qualified as DistributedSet
import TrustlessSidechain.OnChain.Types (
  DsRedeemer (
    DsRedeemer,
    dsStr
  ),
 )
import Prelude qualified

-- | 'logDs' logs the entire distributed set
logDs :: AsContractError e => Ds -> Contract w s e ()
logDs ds = do
  Contract.logInfo @Text (Text.pack ("Logging the distributed set: " Prelude.<> Prelude.show ds))
  Contract.utxosAt (DistributedSet.insertAddress ds) >>= \utxos ->
    Fold.forMOf_ Fold.folded utxos $ \utxo ->
      Contract.logInfo @Text $ Text.pack $ Prelude.show utxo
  Contract.logInfo @Text "Logging the distributed set END"

-- | 'dsToDsMint' is a helper function to convert 'Ds' into 'DsMint'
dsToDsMint :: Ds -> DsMint
dsToDsMint ds =
  DsMint {dsmTxOutRef = dsTxOutRef ds, dsmValidatorHash = DistributedSet.insertValidatorHash ds}

{- | 'findDsOutput' finds the transaction which we must insert to
 (if it exists) for the distributed set.
-}
findDsOutput ::
  forall w s e.
  AsContractError e =>
  Ds ->
  TokenName ->
  Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DsDatum, TokenName))
findDsOutput ds tn =
  queryTn (Value.tokenName "") >>= \case
    Nothing -> return Nothing
    Just v -> fmap Just $ go v
  where
    queryUtxos :: TokenName -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    queryUtxos = Utils.utxosWithCurrency pq . Value.assetClass (DistributedSet.dsCurSymbol $ dsToDsMint ds)

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
              (queryTn . TokenName)
              (DistributedSet.nextNodePrefix str n)
            >>= \case
              Nothing -> return inp
              Just inp' -> go inp'

-- | Converts a 'Node' to the correpsonding 'DsDatum'
nodeToDatum :: Node -> DsDatum
nodeToDatum node =
  DsDatum
    { dsEdge = nEdge node
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

{- | 'sortByOtherListOn lstOrd prj lst' sorts the elements in @lst@ according to
 the total order given by the indices given in @lstOrd@ associated by @prj@.
 If @lst@ contains an element not in @lstOrd@ (which is associated by @prj@),
 then its index is considered to be @Prelude.maxBound@ i.e., this element is
 appended to the end.

 Complexity: /O(n\logn + m\logm)/ for /n,m/ size of @lst@ and @lstOrd@ resp.

 N.B. there's some awkwardness with 'Ord' from Plutus and 'Prelude.Ord'.
 'Map' only works with Prelude's Ord instance.

 N.B. Currently, we don't use this, but the idea was to use this to build
 transactions with the inserted nodes "prenormalized" to make the onchain code
 more efficient. But before the author got to implement this, he found other
 ways to optimize the system which apparently work according to plutip
-}
sortByOtherListOn :: forall a b. Prelude.Ord b => [b] -> (a -> Maybe b) -> [a] -> [a]
sortByOtherListOn lstOrd prj = PreludeList.sortOn (fromMaybe Prelude.maxBound . (toIx PlutusPrelude.<=< prj))
  where
    toIx :: b -> Maybe Prelude.Int
    toIx b = lstOrdToIx Fold.^? At.at b . Prism._Just

    lstOrdToIx :: Map b Prelude.Int
    lstOrdToIx = Map.fromList $ zip lstOrd [0 :: Prelude.Int ..]

-- | 'dsInit' is the offchain code to build the distributed set
dsInit :: DsParams -> Contract () TrustlessSidechainSchema Text AssetClass
dsInit dsp = do
  Contract.txOutFromRef oref >>= \case
    Nothing -> Contract.throwError "error: bad distributed set init unspent transaction doesn't exist."
    Just citxout -> do
      let -- variables creating the token
          sm = DistributedSet.dsCurSymbol dsm
          tn = TokenName $ dspStr dsp
          val = Value.assetClassValue ast 1
          ast = Value.assetClass sm tn

          dat = nodeToDatum DistributedSet.rootNode

          -- lookups required to build the transaction
          lookups =
            Constraints.unspentOutputs (Map.singleton oref citxout)
              Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
              Prelude.<> Constraints.mintingPolicy mp

          -- the transaction
          txConstraints =
            Constraints.mustSpendPubKeyOutput oref
              Prelude.<> Constraints.mustMintValue val
              Prelude.<> Constraints.mustPayToTheScript dat val

      ledgerTx <- Contract.submitTxConstraintsWith @Ds lookups txConstraints

      PlutusPrelude.void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx

      return ast
  where
    -- Aliases:
    oref :: TxOutRef
    oref = dspTxOutRef dsp

    ds :: Ds
    ds = Ds {dsTxOutRef = dspTxOutRef dsp}

    dsm :: DsMint
    dsm = dsToDsMint ds

    mp :: MintingPolicy
    mp = DistributedSet.dsPolicy dsm

-- | 'dsInsert' is the offchain code to build the distributed set
dsInsert :: DsParams -> Contract () TrustlessSidechainSchema Text ()
dsInsert dsp =
  findDsOutput ds (Value.tokenName $ Class.fromBuiltin str) >>= \case
    Nothing -> Contract.throwError "error: distributed set does not exist"
    Just (ref, o, dat, tn) -> do
      let node :: Node
          node = DistributedSet.mkNode (unTokenName tn) dat

          nodes = DistributedSet.insertNode str node

      -- Quickly check if we can really actually insert @str@ into this.
      PlutusTx.Prelude.unless (nPrefix node `DistributedSet.isPrefixOfByteString` str && not (DistributedSet.elemNode str node)) $
        Contract.throwError "error: inserting bad string"

      let lookups =
            Constraints.unspentOutputs (Map.singleton ref o)
              Prelude.<> Constraints.typedValidatorLookups (DistributedSet.typedInsertValidator ds)
              Prelude.<> Constraints.otherScript (DistributedSet.insertValidator ds)
              Prelude.<> Constraints.mintingPolicy mp

          redeemer :: Redeemer
          redeemer = Redeemer $ Class.toBuiltinData $ DsRedeemer {dsStr = str}

          txConstraints =
            Prelude.mconcat
              [ Constraints.mustSpendScriptOutput ref redeemer
              , flip (Fold.foldMapOf Fold.folded) (DistributedSet.toListIb nodes) $
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

      ledgerTx <- Contract.submitTxConstraintsWith @Ds lookups txConstraints

      PlutusPrelude.void $ Contract.awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
  where
    -- Aliases:
    ds :: Ds
    ds = Ds {dsTxOutRef = dspTxOutRef dsp}

    dsm :: DsMint
    dsm = dsToDsMint ds

    mp :: MintingPolicy
    mp = DistributedSet.dsPolicy dsm

    str :: BuiltinByteString
    str = dspStr dsp
