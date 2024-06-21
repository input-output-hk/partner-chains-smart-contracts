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
  , fromMaybeTestError
  ) where

import Contract.Prelude

import Contract.Address (Address, PaymentPubKeyHash)
import Contract.Log as Log
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData (PlutusData(..), fromData)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class as MonadError
import Ctl.Internal.Serialization.Hash as Hash
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Const (Const)
import Data.Function (on)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ord (compare)
import Data.UInt as UInt
import Effect.Exception as Exception
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Mote.Plan as Mote.Plan
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT, throw)
import Test.PlutipTest (PlutipConfigTest, interpretPlutipTest)
import Test.Unit (Test, TestSuite)
import Test.Unit as Test.Unit
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Util as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import Type.Row (type (+))

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
getOwnTransactionInput ∷
  ∀ r. Run (EXCEPT OffchainError + CONTRACT + r) TransactionInput
getOwnTransactionInput = do
  ownUtxos ← fromMaybeThrow (GenericInternalError "Failed to query wallet utxos")
    (liftContract getWalletUtxos)

  case
    List.sortBy
      ( compare `on`
          ( snd >>>
              ( \( TransactionOutputWithRefScript
                     { output: TransactionOutput { amount } }
                 ) → Value.valueToCoin' amount
              )
          )
      )
      (Map.toUnfoldable ownUtxos)
    of
    (Tuple k _) : _ → pure k
    _ → throw (GenericInternalError "No utxo found in wallet")

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

-- fails ∷
--   ∀ r. Run (EXCEPT OffchainError + r) Unit → Run (EXCEPT OffchainError + r) Unit
-- fails contract = do
--   result ← lift _except $ runExcept contract
--   case result of
--     Right _ → throw $ GenericInternalError
--       "Contract should have failed but it didn't."
--     Left e →
--       liftContract $ Log.logInfo'
--         ("Expected failure (and got failure): " <> show e)

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
assertIHaveOutputWithAsset ∷
  ∀ r.
  CurrencySymbol →
  TokenName →
  Run (EXCEPT OffchainError + CONTRACT + r) Unit
assertIHaveOutputWithAsset cs tn = do
  ownUtxos ← map (Map.values) $ Effect.fromMaybeThrow
    (GenericInternalError "Failed to query wallet utxos")
    (liftContract getWalletUtxos)
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

  unless iHaveCurrencySymbolAndTokenName $ throw
    ( GenericInternalError
        $ "Expected me to have at least one asset with currency symbol `"
        <> show cs
        <> "` and token name `"
        <> show tn
        <> "`."
    )

-- | Verifies that a certain script output contains at least one of the given
-- | asset.
assertHasOutputWithAsset ∷
  ∀ r.
  TransactionHash →
  Address →
  CurrencySymbol →
  TokenName →
  Run (EXCEPT OffchainError + CONTRACT + r) Unit
assertHasOutputWithAsset txId addr cs tn = do
  utxos ∷ Array (TransactionInput /\ TransactionOutputWithRefScript) ←
    liftContract $ Map.toUnfoldable <$> utxosAt addr

  unless (any hasAsset utxos) $ throw
    ( GenericInternalError $ "Expected txId `"
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
  , genesisUtxo: toTxIn
      "211307be24c471d42012c5ebd7d98c83f349c612023ce365f9fb5e3e758d0779"
      1
  , thresholdNumerator: BigInt.fromInt 2
  , thresholdDenominator: BigInt.fromInt 3
  , governanceAuthority: Unsafe.unsafePartial $ fromJust $ fromData $ Bytes $
      hexToByteArrayUnsafe
        "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
  }

fromMaybeTestError ∷
  ∀ r a.
  String →
  Run
    (EXCEPT OffchainError + CONTRACT + r)
    (Maybe a) →
  Run
    (EXCEPT OffchainError + CONTRACT + r)
    a
fromMaybeTestError msg = flip bind $ maybe
  ( liftContract $ MonadError.throwError $ Exception.error msg
  )
  pure
