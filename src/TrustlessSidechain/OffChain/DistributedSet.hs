module TrustlessSidechain.OffChain.DistributedSet where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Getter qualified as Getter
import Control.Lens.Indexed qualified as Indexed
import Control.Lens.Prism (_Right)
import Control.Lens.Review qualified as Review
import Control.Lens.Setter qualified as Setter
import Control.Lens.Tuple qualified as Tuple
import Data.Default qualified as Default
import Data.Map (Map)
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
import Plutus.V1.Ledger.Value (TokenName (TokenName, unTokenName))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusPrelude qualified
import PlutusTx.IsData.Class qualified as Class
import PlutusTx.Prelude
import TrustlessSidechain.OffChain.Utils qualified as Utils
import TrustlessSidechain.OnChain.DistributedSet (
  Ds (dsConf),
  DsConfDatum,
  DsDatum (DsDatum, dsBreak, dsOne, dsZero),
  DsMint (DsMint, dsmConf, dsmValidatorHash),
  Node (nBreak, nOne, nPrefix, nZero),
  PBr,
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

-- | 'dsToDsMint' is a helper function to convert 'Ds' into 'DsMint'
dsToDsMint :: Ds -> DsMint
dsToDsMint ds =
  DsMint {dsmConf = dsConf ds, dsmValidatorHash = DistributedSet.insertValidatorHash ds}

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
  queryTn (TokenName $ nPrefix DistributedSet.rootNode, nBreak DistributedSet.rootNode) >>= \case
    Nothing -> return Nothing
    Just v -> fmap Just $ go v
  where
    queryUtxos :: TokenName -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    queryUtxos = Utils.utxosWithCurrency pq . Value.assetClass (DistributedSet.dsPrefixCurSymbol $ dsToDsMint ds)

    queryTn :: (TokenName, PBr) -> Contract w s e (Maybe (TxOutRef, ChainIndexTxOut, DsDatum, TokenName))
    queryTn (inp, br) =
      fmap Indexed.itoList (queryUtxos inp) >>= \utxos ->
        let loop [] = return Nothing
            loop ((ref, o) : ts)
              | Just (dat :: DsDatum) <- Class.fromBuiltinData . getDatum =<< Fold.preview (Tx.ciTxOutDatum . _Right) o
                , dsBreak dat == br
                , Getter.view Tx.ciTxOutAddress o == DistributedSet.insertAddress ds =
                return $ Just (ref, o, dat, inp)
              | otherwise = loop ts
         in loop utxos

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
              (queryTn . Setter.over Tuple._1 TokenName)
              (DistributedSet.nextNodePrefix str n)
            >>= \case
              Nothing -> return inp
              Just inp' -> go inp'

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
    { dsBreak = nBreak node
    , dsZero = nZero node
    , dsOne = nOne node
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
