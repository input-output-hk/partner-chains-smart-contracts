module BalanceTx.Extra where

import Contract.Prelude

import Cardano.Types.Transaction (TransactionOutput(..), _body, _outputs)
import Contract.ScriptLookups (UnattachedUnbalancedTx(..))
import Contract.Transaction (OutputDatum(..), UnbalancedTx(..))
import Data.Lens (lens')
import Data.Lens.Getter ((^.))
import Data.Lens.Setter ((%~), (.~))
import Data.Lens.Traversal (traversed)
import Data.Lens.Types (Lens')
import Data.Map as Map
import Hashing (datumHash) as Hashing
import Types.Datum (Datum)
import Types.OutputDatum (OutputDatum)
import Types.UnbalancedTransaction (UnbalancedTx, _transaction)

--| Making all output datums inline
reattachDatumsInline :: UnattachedUnbalancedTx -> UnattachedUnbalancedTx
reattachDatumsInline utx =
  let
    datums = utx ^. _datums
    datumMap =
      foldl (\dm d -> maybe dm (\dh -> Map.insert dh d dm) (Hashing.datumHash d))
        Map.empty
        datums

    f NoOutputDatum = NoOutputDatum
    f (OutputDatum d) = OutputDatum d
    f (OutputDatumHash dh) = case Map.lookup dh datumMap of
      Nothing -> OutputDatumHash dh -- This should never happen, so we don't handle it explicitly
      Just d -> OutputDatum d
  in
    utx
      #
        ( _unbalancedTx <<< _transaction <<< _body <<< _outputs %~ traversed
            (_datum %~ f)
        )
      # (_datums .~ [])

getOuts :: UnattachedUnbalancedTx -> Array TransactionOutput
getOuts utx = utx ^. _unbalancedTx <<< _transaction <<< _body <<< _outputs

_unbalancedTx :: Lens' UnattachedUnbalancedTx UnbalancedTx
_unbalancedTx = lens' \(UnattachedUnbalancedTx rec@{ unbalancedTx }) ->
  Tuple unbalancedTx \utx -> UnattachedUnbalancedTx rec { unbalancedTx = utx }

_datums ::
  Lens' UnattachedUnbalancedTx (Array Datum)
_datums = lens' \(UnattachedUnbalancedTx rec@{ datums }) ->
  Tuple datums \ds -> UnattachedUnbalancedTx rec { datums = ds }

_datum ::
  Lens' TransactionOutput OutputDatum
_datum = lens' \(TransactionOutput rec@{ datum }) ->
  Tuple datum \d -> TransactionOutput rec { datum = d }
