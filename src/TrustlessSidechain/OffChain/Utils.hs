-- | A collection of some utility functions for the offchain code.
module TrustlessSidechain.OffChain.Utils (foldUtxoRefsWithCurrency, utxosWithCurrency) where

import Control.Lens.Fold qualified as Fold
import Control.Lens.Review qualified as Review
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger (TxOutRef)
import Ledger.Tx (
  ChainIndexTxOut,
 )
import Plutus.ChainIndex (
  Page (nextPageQuery, pageItems),
  PageQuery,
 )
import Plutus.ChainIndex.Api (IsUtxoResponse (isUtxo), UtxosResponse (page))
import Plutus.Contract (AsContractError, Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Effects qualified as Effects
import Plutus.Contract.Request qualified as Request
import Plutus.V1.Ledger.Value (AssetClass)
import PlutusPrelude qualified
import PlutusTx.Prelude

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

-- | 'utxosWithCurrency' queries all the utxos with a given 'AssetClass'
utxosWithCurrency ::
  forall w s e.
  AsContractError e =>
  PageQuery TxOutRef ->
  AssetClass ->
  Contract w s e (Map TxOutRef ChainIndexTxOut)
utxosWithCurrency = foldUtxoRefsWithCurrency go Map.empty
  where
    go :: Map TxOutRef ChainIndexTxOut -> UtxosResponse -> Contract w s e (Map TxOutRef ChainIndexTxOut)
    go acc utxoRsp = (\k -> Fold.foldlMOf Fold.folded k acc (pageItems $ page utxoRsp)) $
      -- Why do we need to use 'Contract.utxoRefMembership ref' to
      -- test if @ref@ is in the utxo set? Apparently (according to
      -- Plutip in the tests) sometimes the 'utxoRefsWithCurrency'
      -- doesn't actually return a utxo (i.e., it returns outputs
      -- that are already spent). This is weird.... very weird..
      \acc' ref ->
        Contract.utxoRefMembership ref >>= \p ->
          Request.txOutFromRef ref PlutusPrelude.<&> \case
            Just o | isUtxo p -> Map.insert ref o acc'
            _ -> acc'
