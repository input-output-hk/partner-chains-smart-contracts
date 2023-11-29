-- | This module contains error data type definitions and algebraic effect
-- | handlers for exceptions.
module TrustlessSidechain.Utils.Error
  ( OffchainError(..)
  , InternalError(..)
  , runInternalError
  , runShowError
  , flattenExcept
  ) where

import Contract.Prelude

import Contract.ScriptLookups as ScriptLookups
import Contract.Transaction as Transaction
import Data.Bifunctor (lmap)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import Type.Row (type (+))

-- | Error raised from the off-chain code of the application
data OffchainError
  -- | Error type for internal errors, with information to developers and
  -- | maintainers
  = InternalError InternalError
  -- | Error occured because of an invalid input, with informaton to the
  -- | end-user
  | InvalidInputError String

derive instance Generic OffchainError _

instance Show OffchainError where
  show = genericShow

-- | Error type for internal errors, with error information to developers and
-- | maintainers
data InternalError
  -- | A UTxO that should exist (not given as user input) could not be found
  = NotFoundUtxo String
  -- | Own payment public key hashes cannot be found
  | NotFoundOwnPubKeyHash
  -- | Own address cannot be found
  | NotFoundOwnAddress
  -- | Invalid policy or validator script, conversion to currency symbol /
  -- | validator hash failed
  | InvalidScript String
  -- | Invalid datum or redeemer, decoding errors
  | InvalidData String
  -- | Conversion of any data type (excluding datums and redeemers, use
  -- | InvalidData for those)
  | ConversionError String
  -- | Error while building a transaction from lookups and constraints
  | BuildTxError ScriptLookups.MkUnbalancedTxError
  -- | Error while attempting to balance a transaction
  | BalanceTxError Transaction.BalanceTxError
  | Other String

derive instance Generic InternalError _

instance Show InternalError where
  show = genericShow

-- | Handle effect of `InternalError` exceptions.
runInternalError ∷ ∀ r. Run (EXCEPT InternalError + AFF + r) ~> Run (AFF + r)
runInternalError r1 = do
  Except.runExcept r1 >>= case _ of
    Right ok → pure ok
    Left err →
      -- TODO: this implementation is equivalent to runShowError.  It should be
      -- replaced with a specialized implementation
      Run.liftAff <<< liftEffect <<< Exception.throw <<< show $ err

-- | Handle exceptions that are instances of `Show`.  Exceptions are converted
-- | to a `String` and thrown as exception in `Aff` embedded in AFF effect.
runShowError ∷ ∀ a r. Show a ⇒ Run (EXCEPT a + AFF + r) ~> Run (AFF + r)
runShowError r1 = do
  res ← Except.runExcept r1
  either (Run.liftAff <<< liftEffect <<< Exception.throw <<< show)
    pure
    res

-- | Flattens a stack of two exceptions, by mapping one into the other.  FIXME:
-- | this functions is of very limited use now, since it demands that exceptions
-- | are directly next to each other in the stack of effects.  This is wrong.
-- | We would like a functions such as Polysemy's `mapError`, that maps one
-- | error into another error present *anywhere* down the effect stack.
flattenExcept ∷
  ∀ a b r. (a → b) → Run (EXCEPT a + EXCEPT b + r) ~> Run (EXCEPT b + r)
flattenExcept f r1 = do
  res ← Except.runExcept r1
  Except.rethrow (f `lmap` res)
