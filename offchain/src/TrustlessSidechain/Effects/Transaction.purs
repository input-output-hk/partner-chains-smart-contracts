module TrustlessSidechain.Effects.Transaction
  ( TRANSACTION
  , TransactionF
  , awaitTxConfirmed
  , balanceTx
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

import Cardano.Types.Address (Address)
import Contract.BalanceTxConstraints (BalanceTxConstraintsBuilder)
import Contract.ScriptLookups (ScriptLookups, UnbalancedTx)
import Contract.ScriptLookups as ScriptLookups
import Contract.UnbalancedTx (MkUnbalancedTxError)
import Contract.UnbalancedTx (mkUnbalancedTx) as UnbalancedTx
import Contract.Transaction
  ( Transaction
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  )
import Contract.Transaction
  ( awaitTxConfirmed
  , balanceTxWithConstraints
  , signTransaction
  , submit
  ) as Transaction
import Contract.Transaction as BalanceTxError
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (UtxoMap)
import Contract.Utxos (getUtxo, utxosAt) as Transaction
import Effect.Aff (Error)
import Run (Run, interpret, on, send)
import Run as Run
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Contract (CONTRACT, withTry, withTryE)
import TrustlessSidechain.Effects.Errors.Context
  ( ErrorContext(ErrorContext)
  , ErrorContextType(Transaction)
  )
import TrustlessSidechain.Effects.Errors.Parser
  ( parseDefaultError
  , parseFromError
  )
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

data TransactionF a
  = UtxosAt Address (UtxoMap → a)
  | GetUtxo TransactionInput (Maybe TransactionOutput → a)
  | MkUnbalancedTx ScriptLookups TxConstraints
      (UnbalancedTx → a)
  | BalanceTxWithConstraints UnbalancedTx BalanceTxConstraintsBuilder
      (Transaction → a)
  | SignTransaction Transaction
      (Transaction → a)
  | Submit Transaction (TransactionHash → a)
  | AwaitTxConfirmed TransactionHash a

derive instance functorTransactionF ∷ Functor TransactionF

type TRANSACTION r = (transaction ∷ TransactionF | r)

_transaction ∷ Proxy "transaction"
_transaction = Proxy

handleTransactionWith ∷
  ∀ r. (TransactionF ~> Run r) → Run (TRANSACTION + r) ~> Run r
handleTransactionWith f = interpret (on _transaction f send)

utxosAt ∷
  ∀ r . Address → Run (TRANSACTION + r) UtxoMap
utxosAt address = Run.lift _transaction
  (UtxosAt address identity)

getUtxo ∷
  ∀ r.
  TransactionInput →
  Run (TRANSACTION + r) (Maybe TransactionOutput)
getUtxo oref = Run.lift _transaction (GetUtxo oref identity)

mkUnbalancedTx ∷
  ∀ r.
  ScriptLookups →
  (TxConstraints) →
  Run (EXCEPT MkUnbalancedTxError + TRANSACTION + r)
    UnbalancedTx
mkUnbalancedTx lookups constraints = Run.lift
  _transaction
  ( MkUnbalancedTx lookups constraints
      identity
  )

balanceTxWithConstraints ∷
  ∀ r.
  UnbalancedTx →
  BalanceTxConstraintsBuilder →
  Run (EXCEPT BalanceTxError.BalanceTxError + TRANSACTION + r)
    Transaction
balanceTxWithConstraints unbalancedTx constraints = Run.lift
  _transaction
  (BalanceTxWithConstraints unbalancedTx constraints identity)

signTransaction ∷
  ∀ r.
  Transaction →
  Run (TRANSACTION + r) Transaction
signTransaction tx = Run.lift _transaction (SignTransaction tx identity)

submit ∷
  ∀ r.
  Transaction →
  Run (TRANSACTION + r) TransactionHash
submit tx = Run.lift _transaction (Submit tx identity)

awaitTxConfirmed ∷
  ∀ r.
  TransactionHash →
  Run (TRANSACTION + r) Unit
awaitTxConfirmed tx = Run.lift _transaction (AwaitTxConfirmed tx unit)

balanceTx ∷
  ∀ r.
  UnbalancedTx →
  Run (EXCEPT BalanceTxError.BalanceTxError + TRANSACTION + r)
    Transaction
balanceTx = flip balanceTxWithConstraints mempty

handleTransactionLive ∷
  ∀ r. TransactionF ~> Run (EXCEPT OffchainError + CONTRACT + r)
handleTransactionLive =
  case _ of
    UtxosAt addr f → f <$> withTry
      (fromError "utxosAt: ")
      (Transaction.utxosAt addr)
    GetUtxo oref f → f <$> withTry (fromError "getUtxo: ")
      (Transaction.getUtxo oref)
    MkUnbalancedTx lookups constraints f →
      f <$> withTry
        (fromError "mkUnabalancedTx: ")
        (UnbalancedTx.mkUnbalancedTx lookups constraints)
    BalanceTxWithConstraints unbalancedTx constraints f →
      f <$> withTry
        (fromError "balancedTxWithConstraints: ")
        (Transaction.balanceTxWithConstraints unbalancedTx constraints)
    SignTransaction tx f → f <$> withTry (fromError "signTransaction: ")
      (Transaction.signTransaction tx)
    Submit tx f → f <$> withTry (fromError "submit: ") (Transaction.submit tx)
    AwaitTxConfirmed tx x → (const x) <$> withTry
      (fromError "awaitTxConfirmed: ")
      (Transaction.awaitTxConfirmed tx)
  where
  fromError ∷ String → Error → OffchainError
  fromError ctx = parseFromError parseDefaultError
    (Just (ErrorContext Transaction ctx))

  fromUnbalanced ∷ MkUnbalancedTxError → OffchainError
  fromUnbalanced = GenericInternalError <<< show

  fromBalanced ∷ BalanceTxError.BalanceTxError → OffchainError
  fromBalanced = GenericInternalError <<< show
