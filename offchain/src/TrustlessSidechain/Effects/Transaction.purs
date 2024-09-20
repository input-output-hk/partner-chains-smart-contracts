module TrustlessSidechain.Effects.Transaction
  ( TRANSACTION
  , TransactionF
  , awaitTxConfirmed
  , balanceTx
  , UnbalancedTx(UnbalancedTx)
  , UnindexedRedeemer(UnindexedRedeemer)
  , RedeemerPurpose(ForSpend, ForMint, ForReward, ForCert)
  , balanceTxWithConstraints
  , getUtxo
  , handleTransactionLive
  , handleTransactionWith
  , mkUnbalancedTx
  , signTransaction
  , submit
  , utxosAt
  ) where

import Contract.Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.Types (Certificate, RewardAddress, ScriptHash)
import Cardano.Types.Address (Address)
import Cardano.Types.PlutusData (PlutusData)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.Monad (Contract)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Transaction
  ( Transaction
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , explainBalanceTxError
  )
import Contract.Transaction (awaitTxConfirmed, signTransaction, submit) as Transaction
import Contract.Transaction as BalanceTxError
import Contract.TxConstraints (TxConstraints)
import Contract.UnbalancedTx (mkUnbalancedTx) as UnbalancedTx
import Contract.Utxos (UtxoMap)
import Contract.Utxos (getUtxo, utxosAt) as Transaction
import Control.Monad.Error.Class (throwError)
import Ctl.Internal.BalanceTx (balanceTxWithConstraints) as Transaction
import Ctl.Internal.ProcessConstraints.Error (MkUnbalancedTxError)
import Data.Map (Map)
import Data.Show.Generic (genericShow)
import Effect.Aff (Error)
import Effect.Exception (error)
import Run (Run, interpret, on, send)
import Run as Run
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Contract (CONTRACT, withTry)
import TrustlessSidechain.Effects.Errors.Context
  ( ErrorContext(ErrorContext)
  , ErrorContextType(Transaction)
  )
import TrustlessSidechain.Effects.Errors.Parser
  ( parseDefaultError
  , parseFromError
  )
import TrustlessSidechain.Error (OffchainError)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

-- | A newtype for the unbalanced transaction after creating one with datums
-- | and redeemers not attached.
newtype UnbalancedTx = UnbalancedTx
  { transaction :: Transaction -- the unbalanced tx created
  , usedUtxos :: Map TransactionInput TransactionOutput
  }

derive instance Generic UnbalancedTx _
derive instance Newtype UnbalancedTx _
derive newtype instance Eq UnbalancedTx

instance Show UnbalancedTx where
  show = genericShow

-- | Redeemer that hasn't yet been indexed, that tracks its purpose info
-- | that is enough to find its index given a `RedeemersContext`.
newtype UnindexedRedeemer = UnindexedRedeemer
  { datum :: PlutusData
  , purpose :: RedeemerPurpose
  }

derive instance Generic UnindexedRedeemer _
derive instance Newtype UnindexedRedeemer _
derive newtype instance Eq UnindexedRedeemer
derive newtype instance EncodeAeson UnindexedRedeemer

instance Show UnindexedRedeemer where
  show = genericShow

-- | Contains a value redeemer corresponds to, different for each possible
-- | `RedeemerTag`.
-- | Allows to uniquely compute redeemer index, given a `RedeemersContext` that
-- | is valid for the transaction.
data RedeemerPurpose
  = ForSpend TransactionInput
  | ForMint ScriptHash
  | ForReward RewardAddress
  | ForCert Certificate

derive instance Generic RedeemerPurpose _
derive instance Eq RedeemerPurpose

instance EncodeAeson RedeemerPurpose where
  encodeAeson = case _ of
    ForSpend txo -> encodeAeson { tag: "ForSpend", value: encodeAeson txo }
    ForMint mps -> encodeAeson { tag: "ForMint", value: encodeAeson mps }
    ForReward addr -> encodeAeson { tag: "ForReward", value: encodeAeson addr }
    ForCert cert -> encodeAeson { tag: "ForCert", value: encodeAeson cert }

instance Show RedeemerPurpose where
  show = genericShow

data TransactionF a
  = UtxosAt Address (UtxoMap -> a)
  | GetUtxo TransactionInput (Maybe TransactionOutput -> a)
  | MkUnbalancedTx ScriptLookups TxConstraints
      (UnbalancedTx -> a)
  | BalanceTxWithConstraints UnbalancedTx BalanceTxConstraintsBuilder
      (Transaction -> a)
  | SignTransaction Transaction
      (Transaction -> a)
  | Submit Transaction (TransactionHash -> a)
  | AwaitTxConfirmed TransactionHash a

derive instance functorTransactionF :: Functor TransactionF

type TRANSACTION r = (transaction :: TransactionF | r)

_transaction :: Proxy "transaction"
_transaction = Proxy

handleTransactionWith ::
  forall r. (TransactionF ~> Run r) -> Run (TRANSACTION + r) ~> Run r
handleTransactionWith f = interpret (on _transaction f send)

utxosAt ::
  forall r. Address -> Run (TRANSACTION + r) UtxoMap
utxosAt address = Run.lift _transaction
  (UtxosAt address identity)

getUtxo ::
  forall r.
  TransactionInput ->
  Run (TRANSACTION + r) (Maybe TransactionOutput)
getUtxo oref = Run.lift _transaction (GetUtxo oref identity)

mkUnbalancedTx ::
  forall r.
  ScriptLookups ->
  (TxConstraints) ->
  Run (EXCEPT MkUnbalancedTxError + TRANSACTION + r)
    UnbalancedTx
mkUnbalancedTx lookups constraints = Run.lift
  _transaction
  ( MkUnbalancedTx lookups constraints
      identity
  )

balanceTxWithConstraints ::
  forall r.
  UnbalancedTx ->
  BalanceTxConstraintsBuilder ->
  Run (EXCEPT BalanceTxError.BalanceTxError + TRANSACTION + r)
    Transaction
balanceTxWithConstraints unbalancedTx constraints = Run.lift
  _transaction
  (BalanceTxWithConstraints unbalancedTx constraints identity)

signTransaction ::
  forall r.
  Transaction ->
  Run (TRANSACTION + r) Transaction
signTransaction tx = Run.lift _transaction (SignTransaction tx identity)

submit ::
  forall r.
  Transaction ->
  Run (TRANSACTION + r) TransactionHash
submit tx = Run.lift _transaction (Submit tx identity)

awaitTxConfirmed ::
  forall r.
  TransactionHash ->
  Run (TRANSACTION + r) Unit
awaitTxConfirmed tx = Run.lift _transaction (AwaitTxConfirmed tx unit)

balanceTx ::
  forall r.
  UnbalancedTx ->
  Run (EXCEPT BalanceTxError.BalanceTxError + TRANSACTION + r)
    Transaction
balanceTx = flip balanceTxWithConstraints mempty

balanceTxWithConstraintsTx ::
  UnbalancedTx ->
  BalanceTxConstraintsBuilder ->
  Contract Transaction
balanceTxWithConstraintsTx unbalancedTx constraintsBuilder =
  Transaction.balanceTxWithConstraints (unwrap unbalancedTx).transaction
    (unwrap unbalancedTx).usedUtxos
    constraintsBuilder >>=
    case _ of
      Left err -> throwError $ error $ explainBalanceTxError err
      Right res -> pure res

handleTransactionLive ::
  forall r. TransactionF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleTransactionLive =
  case _ of
    UtxosAt addr f -> f <$> withTry
      (fromError "utxosAt: ")
      (Transaction.utxosAt addr)
    GetUtxo oref f -> f <$> withTry (fromError "getUtxo: ")
      (Transaction.getUtxo oref)
    MkUnbalancedTx lookups constraints f ->
      f <$> withTry
        (fromError "mkUnabalancedTx: ")
        (toUnbalancedTx <$> UnbalancedTx.mkUnbalancedTx lookups constraints)
    BalanceTxWithConstraints unbalancedTx constraints f ->
      f <$> withTry
        (fromError "balancedTxWithConstraints: ")
        ( balanceTxWithConstraintsTx unbalancedTx constraints
        )
    SignTransaction tx f -> f <$> withTry (fromError "signTransaction: ")
      (Transaction.signTransaction tx)
    Submit tx f -> f <$> withTry (fromError "submit: ") (Transaction.submit tx)
    AwaitTxConfirmed tx x -> (const x) <$> withTry
      (fromError "awaitTxConfirmed: ")
      (Transaction.awaitTxConfirmed tx)
  where
  fromError :: String -> Error -> OffchainError
  fromError ctx = parseFromError parseDefaultError
    (Just (ErrorContext Transaction ctx))

  toUnbalancedTx :: (Transaction /\ UtxoMap) -> UnbalancedTx
  toUnbalancedTx (tx /\ utxoMap) = UnbalancedTx
    { transaction: tx
    , usedUtxos: utxoMap
    }
