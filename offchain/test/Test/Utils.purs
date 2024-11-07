module Test.Utils
  ( toTxIn
  , getUniqueUtxoAt
  , paymentPubKeyHashToByteArray
  , getOwnTransactionInput
  , fails
  , unsafeBigIntFromString
  , interpretPureTest
  , PureTest
  , TestnetTest
  , assertHasOutputWithAsset
  , assertIHaveOutputWithAsset
  , dummyGenesisUtxo
  , fromMaybeTestError
  , withSingleMultiSig
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Serialization.Lib as CSL
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Contract.Address (Address)
import Contract.Log as Log
import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Test (ContractTest)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Error.Class as MonadError
import Data.Const (Const)
import Data.Function (on)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Maybe as Maybe
import Data.Ord (compare)
import Data.UInt as UInt
import Effect.Exception as Exception
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Mote.Plan as Mote.Plan
import Mote.TestPlanM (TestPlanM)
import Partial.Unsafe (unsafePartial)
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT, throw)
import Run.Reader (local)
import Test.Unit (Test, TestSuite)
import Test.Unit as Test.Unit
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Util as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance (Governance(MultiSig))
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  )
import Type.Row (type (+))

type TestnetTest = TestPlanM ContractTest Unit
type PureTest = Mote (Const Void) Test Unit

toTxIn :: String -> Int -> TransactionInput
toTxIn txId txIdx =
  TransactionInput
    { transactionId: TransactionHash $ unsafePartial $ fromJust $ CSL.fromBytes $
        hexToByteArrayUnsafe txId
    , index: UInt.fromInt txIdx
    }

-- | `getUniqueUtxoAt addr` gets the first utxo at the given address, and throws an
-- | error if there is NOT exactly one utxo at this address.
getUniqueUtxoAt ::
  Address -> Contract (Tuple TransactionInput TransactionOutput)
getUniqueUtxoAt addr = do
  utxoMap <- utxosAt addr
  let
    err = Monad.throwContractError
      $ "Expected exactly one script address but got:"
      <> show utxoMap

  case Map.findMin utxoMap of
    Just { key, value }
      | length utxoMap == 1 -> pure $ key /\ value
      | otherwise -> err
    Nothing -> err

-- | Coerces a `PaymentPubKeyHash` to a `ByteArray`. This is useful when making
-- | the recipient for the `MerkleTreeEntry`.
-- | TODO: the "useful" part is a bit outdated -- recipients in
-- | `MerkleTreeEntry` should actually be bech32 encoded addresses intead of
-- | just the raw pubkey hash
paymentPubKeyHashToByteArray :: PaymentPubKeyHash -> ByteArray
paymentPubKeyHashToByteArray =
  unwrap <<< encodeCbor <<< unwrap

-- | `getOwnTransactionInput` gets a single aribtrary `TransactionInput` from
-- | the current key wallet.
-- | This throws an error if such a utxo does not exist.
-- | This is useful for e.g. initializing the sidechain because we need to mint
-- | an NFT for the initial committee
getOwnTransactionInput ::
  forall r. Run (EXCEPT OffchainError + CONTRACT + r) TransactionInput
getOwnTransactionInput = do
  ownUtxos <- fromMaybeThrow
    (GenericInternalError "Failed to query wallet utxos")
    (liftContract getWalletUtxos)

  case
    List.sortBy
      ( compare `on`
          ( snd >>>
              (\(TransactionOutput { amount }) -> Value.valueToCoin amount)
          )
      )
      (Map.toUnfoldable ownUtxos)
    of
    (Tuple k _) : _ -> pure k
    _ -> throw (GenericInternalError "No utxo found in wallet")

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
fails :: Contract Unit -> Contract Unit
fails contract = do
  result <- MonadError.try contract
  case result of
    Right _ -> Monad.throwContractError $ Exception.error
      "Contract should have failed but it didn't."
    Left e ->
      Log.logInfo' ("Expected failure (and got failure): " <> Exception.message e)

-- | Unsafely converts a String to a BigInt
unsafeBigIntFromString :: String -> BigInt
unsafeBigIntFromString str = Unsafe.unsafePartial Maybe.fromJust
  (BigInt.fromString str)

-- | `interpretPureTest` interprets a standard collection of `PureTest`
-- | and converts this to a `TestSuite`. Following this function with `Test.Unit.Main.runTest`
-- | will run the tests.
interpretPureTest :: PureTest -> TestSuite
interpretPureTest = go <<< Mote.Monad.plan
  where
  go = Mote.Plan.foldPlan
    (\{ label, value } -> Test.Unit.test label value)
    (\label -> Test.Unit.testSkip label (pure unit))
    (\{ label, value } -> Test.Unit.suite label (go value))
    sequence_

-- | `assertIHaveOutputWithAsset` asserts that of all `getWalletUtxos`, there
-- | exists a UTxO with at least one of the given asset.
assertIHaveOutputWithAsset ::
  forall r.
  Asset ->
  Run (EXCEPT OffchainError + CONTRACT + r) Unit
assertIHaveOutputWithAsset asset = do
  ownUtxos <- map (Map.values) $ Effect.fromMaybeThrow
    (GenericInternalError "Failed to query wallet utxos")
    (liftContract getWalletUtxos)
  let
    iHaveCurrencySymbolAndTokenName =
      let
        go input = case List.uncons input of
          Nothing -> false
          Just { head: TransactionOutput { amount }, tail } ->
            if Value.valueOf asset amount > BigNum.zero then true
            else go tail
      in
        go ownUtxos

  unless iHaveCurrencySymbolAndTokenName $ throw
    ( GenericInternalError
        $ "Expected me to have at least one asset "
        <> show asset
        <> "`."
    )

-- | Verifies that a certain script output contains at least one of the given
-- | asset.
assertHasOutputWithAsset ::
  forall r.
  TransactionHash ->
  Address ->
  CurrencySymbol ->
  TokenName ->
  Run (EXCEPT OffchainError + CONTRACT + r) Unit
assertHasOutputWithAsset txId addr cs tn = do
  utxos :: Array (TransactionInput /\ TransactionOutput) <-
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
  hasAsset :: (TransactionInput /\ TransactionOutput) -> Boolean
  hasAsset
    ( TransactionInput { transactionId } /\ TransactionOutput { amount }
    ) =
    transactionId == txId
      && (Value.valueOf (Asset cs tn) amount > BigNum.zero)

-- | `dummyGenesisUtxo` is some default UTXO which may be
-- | helpful when creating tests.
dummyGenesisUtxo :: TransactionInput
dummyGenesisUtxo =
  toTxIn
    "211307be24c471d42012c5ebd7d98c83f349c612023ce365f9fb5e3e758d0779"
    1

fromMaybeTestError ::
  forall r a.
  String ->
  Run
    (EXCEPT OffchainError + CONTRACT + r)
    (Maybe a) ->
  Run
    (EXCEPT OffchainError + CONTRACT + r)
    a
fromMaybeTestError msg = flip bind $ maybe
  ( liftContract $ MonadError.throwError $ Exception.error msg
  )
  pure

withSingleMultiSig ::
  forall r a.
  Ed25519KeyHash ->
  Run (READER Env + r) a ->
  Run (READER Env + r) a
withSingleMultiSig wallet = local $ const
  { governance: Just $ MultiSig $ MultiSigGovParams
      { governanceMembers: [ wallet ]
      , requiredSignatures: BigInt.fromInt 1
      }
  }
