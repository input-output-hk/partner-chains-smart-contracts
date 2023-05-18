module Test.Utils
  ( toTxIn
  , getUniqueUtxoAt
  , paymentPubKeyHashToByteArray
  -- , assertMaxFee
  , getOwnTransactionInput
  , fails
  , unsafeBigIntFromString
  , interpretConstVoidTest
  , interpretWrappedTest
  , pureGroup
  , plutipGroup
  , WithTestRunner(..)
  , WrappedTests
  , assertHasOutputWithAsset
  , assertIHaveOutputWithAsset
  , dummySidechainParams
  ) where

import Contract.Prelude

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Log as Log
import Contract.Monad (Contract, throwContractError)
import Contract.Monad as Monad
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Control.Monad.Error.Class as MonadError
import Ctl.Internal.Serialization.Hash as Hash
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Const (Const)
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set
import Data.UInt as UInt
import Effect.Exception as Exception
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Mote.Plan as Mote.Plan
import Partial.Unsafe as Unsafe
import Test.PlutipTest (PlutipConfigTest, interpretPlutipTest)
import Test.Unit (Test, TestSuite)
import Test.Unit as Test.Unit
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))

toTxIn ∷ String → Int → TransactionInput
toTxIn txId txIdx =
  TransactionInput
    { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
    , index: UInt.fromInt txIdx
    }

-- | `getUniqueUtxoAt addr` gets the first utxo at the given address, and throws an
-- | error if there is NOT exactly one utxo at this address.
getUniqueUtxoAt ∷
  Address → Contract (Tuple TransactionInput TransactionOutputWithRefScript)
getUniqueUtxoAt addr = do
  utxoMap ← utxosAt addr
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
getOwnTransactionInput ∷ Contract TransactionInput
getOwnTransactionInput = do
  ownUtxos ← Monad.liftedM "Failed to query wallet utxos" getWalletUtxos
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
fails ∷ Contract Unit → Contract Unit
fails contract = do
  result ← MonadError.try contract
  case result of
    Right _ → Monad.throwContractError $ Exception.error
      "Contract should have failed but it didn't."
    Left e →
      Log.logInfo' ("Expected failure (and got failure): " <> Exception.message e)

-- | Unsafely converts a String to a BigInt
unsafeBigIntFromString ∷ String → BigInt
unsafeBigIntFromString str = Unsafe.unsafePartial Maybe.fromJust
  (BigInt.fromString str)

-- | `interpretConstVoidTest` interprets a standard collection of `Mote (Const Void) Test Unit`
-- | and converts this to a `TestSuite`. Following this function with `Test.Unit.Main.runTest`
-- | will run the tests.
interpretConstVoidTest ∷ Mote (Const Void) Test Unit → TestSuite
interpretConstVoidTest = go <<< Mote.Monad.plan
  where
  go = Mote.Plan.foldPlan
    (\{ label, value } → Test.Unit.test label value)
    (\label → Test.Unit.testSkip label (pure unit))
    (\{ label, value } → Test.Unit.suite label (go value))
    sequence_

-- TODO: getTxByHash is removed, find a way to implent this
-- | Verifies that the fees of a certain transaction does
-- | not exceed a given amount, it throws an effor otherwise
-- assertMaxFee ∷ ∀ (r ∷ Row Type). BigInt → TransactionHash → Contract Unit
-- assertMaxFee maxFee txId = do
--   Transaction tx ← liftedM "Couldn't find transaction." $ getTxByHash txId
--   let fee = (unwrap (unwrap tx.body).fee)
--   when (fee > maxFee) $ throwContractError
--     ( "Expected transaction fee to be less than "
--         <> BigInt.toString maxFee
--         <> " lovelaces, but it was "
--         <> BigInt.toString fee
--         <> " lovelaces."
--     )

-- | Test wrapper, to distinguish between different test interpreters
data WithTestRunner
  = WithPlutipRunner (Mote (Const Void) PlutipConfigTest Unit)
  | PureRunner (Mote (Const Void) Test Unit)

-- | A type synonym for wrapped tests
type WrappedTests = Mote (Const Void) WithTestRunner Unit

-- | Interpreting wrapped tests with their respective interpreters
interpretWrappedTest ∷ WrappedTests → TestSuite
interpretWrappedTest = go <<< Mote.Monad.plan
  where
  go =
    Mote.Plan.foldPlan
      ( \{ label, value } → Test.Unit.suite label $
          case value of
            WithPlutipRunner testCase → interpretPlutipTest testCase
            PureRunner testCase → interpretConstVoidTest testCase

      )
      (\label → Test.Unit.testSkip label (pure unit))
      (\{ label, value } → Test.Unit.suite label (go value))
      sequence_

-- | A test group function to conveniently wrap multiple Mote tests using `WithTestRunner`
-- | Tests in this group will be executed by Plutip
plutipGroup ∷ String → Mote (Const Void) PlutipConfigTest Unit → WrappedTests
plutipGroup label tests =
  Mote.Monad.test label $ WithPlutipRunner tests

-- | A test group function to conveniently wrap multiple Mote tests using `WithTestRunner`
-- | Tests in this group will be executed purely
pureGroup ∷ String → Mote (Const Void) Test Unit → WrappedTests
pureGroup label tests =
  Mote.Monad.test label $ PureRunner tests

-- | `assertIHaveOutputWithAsset` asserts that of all `getWalletUtxos`, there
-- | exists a UTxO with at least one of the given asset.
assertIHaveOutputWithAsset ∷ CurrencySymbol → TokenName → Contract Unit
assertIHaveOutputWithAsset cs tn = do
  ownUtxos ← map (Map.values) $ Monad.liftedM "Failed to query wallet utxos"
    getWalletUtxos
  let
    iHaveCurrencySymbolAndTokenName =
      let
        go input = case List.uncons input of
          Nothing → false
          Just { head, tail } →
            let
              TransactionOutputWithRefScript { output: TransactionOutput txOut } =
                head
            in
              if Value.valueOf txOut.amount cs tn > zero then true
              else go tail
      in
        go ownUtxos

  unless iHaveCurrencySymbolAndTokenName $ throwContractError
    ( "Expected me to have at least one asset with currency symbol `"
        <> show cs
        <> "` and token name `"
        <> show tn
        <> "`."
    )

-- | Verifies that a certain script output contains at least one of the given
-- | asset.
assertHasOutputWithAsset ∷
  TransactionHash → Address → CurrencySymbol → TokenName → Contract Unit
assertHasOutputWithAsset txId addr cs tn = do
  utxos ∷ Array (TransactionInput /\ TransactionOutputWithRefScript) ←
    Map.toUnfoldable <$> utxosAt addr

  unless (any hasAsset utxos) $ throwContractError
    ( "Expected txId `"
        <> show txId
        <> "` to have an address `"
        <> show addr
        <> "` with at least one asset with currency symbol `"
        <> show cs
        <> "` and token name `"
        <> show tn
        <> "`."
    )

  where
  hasAsset ∷ (TransactionInput /\ TransactionOutputWithRefScript) → Boolean
  hasAsset
    ( TransactionInput { transactionId } /\ TransactionOutputWithRefScript
        { output: TransactionOutput txOut }
    ) =
    transactionId == txId
      && (Value.valueOf txOut.amount cs tn > zero)

-- | `dummySidechainParams` is some default sidechain parameters which may be
-- | helpful when creating tests.
dummySidechainParams ∷ SidechainParams
dummySidechainParams = SidechainParams
  { chainId: BigInt.fromInt 69
  , genesisHash: hexToByteArrayUnsafe "112233"
  , genesisUtxo: toTxIn
      "211307be24c471d42012c5ebd7d98c83f349c612023ce365f9fb5e3e758d0779"
      1
  , thresholdNumerator: BigInt.fromInt 2
  , thresholdDenominator: BigInt.fromInt 3
  }
