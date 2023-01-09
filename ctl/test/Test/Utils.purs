module Test.Utils
  ( toTxIn
  , getUniqueUtxoAt
  , paymentPubKeyHashToByteArray
  , assertMaxFee
  , getOwnTransactionInput
  , fails
  , assertBy
  , unsafeBigIntFromString
  ) where

import Contract.Prelude

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Log as Log
import Contract.Monad (Contract, liftedM, throwContractError)
import Contract.Monad as Monad
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Transaction
  ( Transaction(..)
  , TransactionHash(..)
  , TransactionInput(..)
  , TransactionOutputWithRefScript
  , getTxByHash
  )
import Contract.Utxos as Utxos
import Control.Monad.Error.Class as MonadError
import Ctl.Internal.Serialization.Hash as Hash
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Data.UInt as UInt
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Partial.Unsafe as Unsafe

toTxIn ∷ String → Int → TransactionInput
toTxIn txId txIdx =
  TransactionInput
    { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
    , index: UInt.fromInt txIdx
    }

-- | `getUniqueUtxoAt addr` gets the first utxo at the given address, and throws an
-- | error if there is NOT exactly one utxo at this address.
getUniqueUtxoAt ∷
  Address → Contract () (Tuple TransactionInput TransactionOutputWithRefScript)
getUniqueUtxoAt addr = do
  utxoMap ← Monad.liftedM "Failed to get utxos at script address" $
    Utxos.utxosAt addr
  let
    err = Monad.throwContractError
      $ "Expected exactly one script address but got:"
      <> show utxoMap

  case Map.findMin utxoMap of
    Just { key, value }
      | length utxoMap == 1 → pure $ key /\ value
      | otherwise → err
    Nothing → err

-- | Coerces a `PaymentPubKeyHash` to a `ByteArray`. This is useful when making
-- | the recipient for the `MerkleTreeEntry`.
-- | TODO: the "useful" part is a bit outdated -- recipients in
-- | `MerkleTreeEntry` should actually be bech32 encoded addresses intead of
-- | just the raw pubkey hash
paymentPubKeyHashToByteArray ∷ PaymentPubKeyHash → ByteArray
paymentPubKeyHashToByteArray =
  unwrap <<< Hash.ed25519KeyHashToBytes <<< unwrap <<< unwrap

-- | `getOwnTransactionInput` gets a single aribtrary `TransactionInput` from
-- | the current key wallet.
-- | This throws an error if such a utxo does not exist.
-- | This is useful for e.g. initializing the sidechain because we need to mint
-- | an NFT for the initial committee
getOwnTransactionInput ∷ Contract () TransactionInput
getOwnTransactionInput = do
  ownUtxos ← Monad.liftedM "Failed to query wallet utxos" Utxos.getWalletUtxos
  Monad.liftContractM "No utxo found in wallet"
    $ Set.findMin
    $ Map.keys ownUtxos

-- | `fails contract` executes `contract`, and
-- |
-- |  - If `contract` throws an exception, then the program continues as usual
-- |
-- |  - If `contract` doesn't thrown an exception, then we throw an exception.
-- |
-- | This is used to run tests on programs that _should_ fail e.g. to test if
-- | `myTest` fails, we should write
-- | ```
-- | Test.Utils.fails myTest
-- | ```
fails ∷ Contract () Unit → Contract () Unit
fails contract = do
  result ← MonadError.try contract
  case result of
    Right _ → Monad.throwContractError $ Exception.error
      "Contract should have failed but it didn't."
    Left e →
      Log.logInfo' ("Expected failure (and got failure): " <> Exception.message e)

-- | `assertBy eqBy expected actual` does nothing if `eqBy expected actual == true`,
-- | and logs and throws an exception otherwise.
assertBy ∷ ∀ a. Show a ⇒ (a → a → Boolean) → a → a → Effect Unit
assertBy eqBy expected actual =
  if eqBy expected actual then pure unit
  else do
    Console.warn "Assertion failed!"
    Console.warn "Expected:"
    Console.warnShow expected
    Console.warn "But got:"
    Console.warnShow actual
    Exception.throwException (Exception.error "Test case failed!")

-- | Unsafely converts a String to a BigInt
unsafeBigIntFromString ∷ String → BigInt
unsafeBigIntFromString str = Unsafe.unsafePartial Maybe.fromJust
  (BigInt.fromString str)

assertMaxFee ∷ ∀ (r ∷ Row Type). BigInt → TransactionHash → Contract () Unit
assertMaxFee maxFee txId = do
  Transaction tx ← liftedM "Couldn't find transaction." $ getTxByHash txId
  let fee = (unwrap (unwrap tx.body).fee)
  when (fee > maxFee) $ throwContractError
    ( "Expected transaction fee to be less than "
        <> BigInt.toString maxFee
        <> " lovelaces, but it was "
        <> BigInt.toString fee
        <> " lovelaces."
    )
