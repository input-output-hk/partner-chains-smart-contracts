module Test.Reserve
  ( suite
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib (fromBytes)
import Cardano.Types.Asset (Asset(AdaAsset), fromAssetClass)
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef(PlutusScriptRef))
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.Value (getCoin, lovelaceValueOf, valueOf)
import Cardano.Types.Value as Value
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Test.Testnet (withWallets)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Map as Map
import Effect.Exception (error)
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Partial.Unsafe (unsafePartial)
import Run (EFFECT, Run)
import Run.Except (EXCEPT)
import Test.AlwaysPassingScripts (alwaysPassingPolicy)
import Test.Utils (TestnetTest)
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Log (LOG, logInfo')
import TrustlessSidechain.Effects.Run (unliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Governance (initGovernance)
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.NativeTokenManagement.Reserve
  ( depositToReserve
  , extractReserveDatum
  , findReserveUtxos
  , handover
  , initialiseReserveUtxo
  , transferToIlliquidCirculationSupply
  , updateReserveUtxo
  )
import TrustlessSidechain.NativeTokenManagement.Types
  ( ImmutableReserveSettings(ImmutableReserveSettings)
  , MutableReserveSettings(MutableReserveSettings)
  )
import TrustlessSidechain.Utils.Address
  ( getOwnPaymentPubKeyHash
  , getOwnWalletAddress
  )
import TrustlessSidechain.Utils.Asset (emptyAssetName)
import TrustlessSidechain.Utils.Data
  ( VersionedGenericDatum(VersionedGenericDatum)
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(IlliquidCirculationSupplyValidator)
  )
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

invalidScriptHash :: ScriptHash
invalidScriptHash =
  wrap $ unsafePartial $ fromJust $ fromBytes $ hexToByteArrayUnsafe
    "00000000000000000000000000000000000000000000000000000000"

immutableAdaSettings :: ImmutableReserveSettings
immutableAdaSettings = ImmutableReserveSettings
  { t0: zero
  , tokenKind: AdaAsset
  }

suite :: TestnetTest
suite = group "Reserve" do
  testScenario3
  testScenario4
  testScenario5
  testScenario6
  testScenario7
  testScenario8

totalAssets :: forall f. Foldable f => f (TransactionOutput) -> Value.Value
totalAssets assets = unsafePartial $ fromJust $ Value.sum
  $ map
      (unwrap >>> _.amount)
  $ Array.fromFoldable assets

invalidMutableSettings :: MutableReserveSettings
invalidMutableSettings = MutableReserveSettings
  { vFunctionTotalAccrued: invalidScriptHash
  , incentiveAmount: BigInt.fromInt (-1)
  }

dummyInitialiseSidechain ::
  forall r.
  PaymentPubKeyHash ->
  Run
    (APP + EFFECT + CONTRACT + r)
    TransactionInput
dummyInitialiseSidechain pkh = do
  genesisUtxo <- Test.Utils.getOwnTransactionInput

  _ <- initGovernance genesisUtxo pkh
  _ <- initNativeTokenMgmt genesisUtxo

  pure genesisUtxo

findIlliquidCirculationSupplyUtxos ::
  forall r.
  TransactionInput ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    UtxoMap
findIlliquidCirculationSupplyUtxos genesisUtxo =
  Versioning.getVersionedValidatorAddress
    genesisUtxo
    ( VersionOracle
        { scriptId: IlliquidCirculationSupplyValidator
        }
    )
    >>= utxosAt

mintNonAdaTokens ::
  forall r.
  Int.Int ->
  Run
    (EXCEPT OffchainError + LOG + WALLET + TRANSACTION + r)
    AssetClass
mintNonAdaTokens numOfTokens = do
  policy <- alwaysPassingPolicy $ BigInt.fromInt 100

  let
    cs = PlutusScript.hash policy

    lookups = Lookups.plutusMintingPolicy policy

    constraints =
      TxConstraints.mustMintValue
        (Mint.singleton cs emptyAssetName numOfTokens)

  void $ balanceSignAndSubmit
    "Reserve initialization transaction"
    { constraints, lookups }

  pure $ AssetClass cs emptyAssetName

initialDistribution :: Array BigNum
initialDistribution =
  [ BigNum.fromInt 50_000_000
  , BigNum.fromInt 50_000_000
  , BigNum.fromInt 50_000_000
  , BigNum.fromInt 40_000_000
  , BigNum.fromInt 40_000_000
  ]

testScenario3 :: TestnetTest
testScenario3 =
  test "Deposit more non-ADA to a reserve" do
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp do
        pkh <- getOwnPaymentPubKeyHash
        do

          genesisUtxo <- dummyInitialiseSidechain pkh

          let
            initialAmountOfNonAdaTokens = 50
            depositAmountOfNonAdaTokens = 51

            numOfNonAdaTokens =
              (initialAmountOfNonAdaTokens + depositAmountOfNonAdaTokens)

          tokenKind <- mintNonAdaTokens $ Int.fromInt numOfNonAdaTokens

          let
            immutableSettings = ImmutableReserveSettings
              { t0: zero
              , tokenKind: fromAssetClass tokenKind
              }

          logInfo' "aaaa initialized"
          void $ initialiseReserveUtxo
            genesisUtxo
            immutableSettings
            invalidMutableSettings
            (BigNum.fromInt initialAmountOfNonAdaTokens)
          logInfo' "Reserve initialized"
          void $ depositToReserve
            genesisUtxo
            (fromAssetClass tokenKind)
            (BigNum.fromInt depositAmountOfNonAdaTokens)

          maybeUtxo <- Map.toUnfoldable
            <$> findReserveUtxos genesisUtxo

          let
            extractValue = snd >>> unwrap >>> _.amount
            isExpectedAmount = valueOf (fromAssetClass tokenKind)

          unless
            ( Just (BigNum.fromInt numOfNonAdaTokens) ==
                (extractValue >>> isExpectedAmount <$> maybeUtxo)
            )
            (liftContract $ throwError $ error "Deposit not sucessful")

testScenario4 :: TestnetTest
testScenario4 =
  test
    "Update reserve utxo mutable settings"
    do
      withWallets initialDistribution \alice -> do
        withKeyWallet alice $ unliftApp do
          pkh <- getOwnPaymentPubKeyHash
          do

            genesisUtxo <- dummyInitialiseSidechain pkh

            void $ initialiseReserveUtxo
              genesisUtxo
              immutableAdaSettings
              invalidMutableSettings
              (BigNum.fromInt 2_000_000)

            utxoBefore <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found" $
                (Map.toUnfoldable <$> findReserveUtxos genesisUtxo)

            let
              updatedMutableSettings = MutableReserveSettings
                { vFunctionTotalAccrued:
                    wrap $ unsafePartial $ fromJust $ fromBytes
                      $ hexToByteArrayUnsafe
                      $
                        "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
                , incentiveAmount: BigInt.fromInt 20
                }

            void $ updateReserveUtxo
              genesisUtxo
              updatedMutableSettings
              utxoBefore

            utxoAfter <-
              Test.Utils.fromMaybeTestError "Utxo after update not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos genesisUtxo

            let
              getReserveDatum =
                snd
                  >>> extractReserveDatum

              withUpdatedMutableSettings
                (VersionedGenericDatum { datum, builtinData, version }) =
                VersionedGenericDatum
                  { datum: wrap $ (unwrap datum)
                      { mutableSettings = updatedMutableSettings }
                  , builtinData
                  , version
                  }

            unless
              ( (withUpdatedMutableSettings <$> getReserveDatum utxoBefore)
                  == getReserveDatum utxoAfter
              )
              (liftContract $ throwError $ error "Update not sucessful")

getFakeVt ::
  forall r.
  Run (APP + r)
    { input :: TransactionInput
    , policy :: PlutusScript.PlutusScript
    }
getFakeVt = do
  policy <- alwaysPassingPolicy $ BigInt.fromInt 11
  ownPkh <- getOwnPaymentPubKeyHash
  ownAddr <- getOwnWalletAddress
  let
    constraints = TxConstraints.mustPayToPubKeyWithScriptRef ownPkh
      (PlutusScriptRef policy)
      (lovelaceValueOf $ BigNum.fromInt 1000000)
  _ <- balanceSignAndSubmit "Fake VT" { lookups: mempty, constraints }

  ownUtxos <- utxosAt ownAddr
  let
    (input /\ _) = unsafePartial $ fromJust
      $ Array.find
          ( \(_ /\ TransactionOutput { scriptRef }) ->
              case scriptRef of
                Nothing -> false
                Just (PlutusScriptRef ref) -> ref == policy
          )
      $ Map.toUnfoldable ownUtxos

  pure $ { input, policy }

testScenario5 :: TestnetTest
testScenario5 =
  test
    "Transfer to illiquid circulation supply with non-ADA as reserve token"
    do
      withWallets initialDistribution \alice -> do
        withKeyWallet alice $ unliftApp do
          pkh <- getOwnPaymentPubKeyHash
          do
            genesisUtxo <- dummyInitialiseSidechain pkh

            let
              numOfNonAdaTokens = 101
              numOfTransferTokens = 15
            tokenKind <- mintNonAdaTokens $ Int.fromInt numOfNonAdaTokens

            { input: fakeVtInput, policy: fakeVt } <- getFakeVt

            let
              incentiveAmount = 1

              mutableSettings = MutableReserveSettings
                { vFunctionTotalAccrued: PlutusScript.hash fakeVt
                , incentiveAmount: BigInt.fromInt incentiveAmount
                }

              immutableSettings = ImmutableReserveSettings
                { t0: zero
                , tokenKind: fromAssetClass tokenKind
                }

            void $ initialiseReserveUtxo
              genesisUtxo
              immutableSettings
              mutableSettings
              (BigNum.fromInt numOfNonAdaTokens)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos genesisUtxo

            void $ transferToIlliquidCirculationSupply
              genesisUtxo
              numOfTransferTokens
              fakeVtInput
              utxo

            reserveAfterTransfer <- totalAssets <$> findReserveUtxos
              genesisUtxo
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                genesisUtxo

            let amountOfReserveTokens t = valueOf (fromAssetClass tokenKind) t
            unless
              ( amountOfReserveTokens reserveAfterTransfer ==
                  BigNum.fromInt (numOfNonAdaTokens - numOfTransferTokens)
              )
              ( liftContract $ throwError $ error
                  "Incorrect number of reserve tokens in reserve after transfer"
              )

            unless
              ( amountOfReserveTokens icsAfterTransfer ==
                  BigNum.fromInt (numOfTransferTokens - incentiveAmount)
              )
              ( liftContract $ throwError $ error
                  "Incorrect number of reserve tokens in ICS after transfer"
              )

            Test.Utils.assertIHaveOutputWithAsset $ fromAssetClass tokenKind

            pure unit

testScenario8 :: TestnetTest
testScenario8 =
  test
    "Transfer to illiquid circulation supply with ADA as reserve token"
    do
      withWallets initialDistribution \alice -> do
        withKeyWallet alice $ unliftApp do
          pkh <- getOwnPaymentPubKeyHash
          do

            genesisUtxo <- dummyInitialiseSidechain pkh

            { input: fakeVtInput, policy: fakeVt } <- getFakeVt

            let
              numOfAda = 5_000_000
              numOfTransferred = 3_000_000
              incentiveAmount = BigInt.fromInt 0
              mutableSettings = MutableReserveSettings
                { vFunctionTotalAccrued:
                    PlutusScript.hash
                      $ fakeVt
                , incentiveAmount
                }

            void $ initialiseReserveUtxo
              genesisUtxo
              immutableAdaSettings
              mutableSettings
              (BigNum.fromInt numOfAda)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos genesisUtxo

            void $ transferToIlliquidCirculationSupply
              genesisUtxo
              numOfTransferred
              fakeVtInput
              utxo

            let amountOfReserveTokens t = unwrap $ getCoin t

            reserveAfterTransfer <- totalAssets <$> findReserveUtxos
              genesisUtxo
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                genesisUtxo

            unless
              ( amountOfReserveTokens reserveAfterTransfer ==
                  BigNum.fromInt (numOfAda - numOfTransferred)
              )
              ( liftContract $ throwError $ error
                  "Incorrect number of reserve tokens in reserve after transfer"
              )

            unless
              ( amountOfReserveTokens icsAfterTransfer == BigNum.fromInt
                  numOfTransferred
              )
              ( liftContract $ throwError $ error
                  "Incorrect number of reserve tokens in ICS after transfer"
              )

            pure unit

testScenario6 :: TestnetTest
testScenario6 =
  test
    "Handover with non-ADA as reserve token"
    do
      withWallets initialDistribution \alice -> do
        withKeyWallet alice $ unliftApp do

          pkh <- getOwnPaymentPubKeyHash
          do
            genesisUtxo <- dummyInitialiseSidechain pkh

            let numOfNonAdaTokens = 101

            tokenKind <- mintNonAdaTokens $ Int.fromInt numOfNonAdaTokens

            let
              immutableSettings = ImmutableReserveSettings
                { t0: zero
                , tokenKind: fromAssetClass tokenKind
                }

            void $ initialiseReserveUtxo
              genesisUtxo
              immutableSettings
              invalidMutableSettings
              (BigNum.fromInt numOfNonAdaTokens)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos genesisUtxo

            void $ handover
              genesisUtxo
              utxo

            reserveUtxosAfterHandover <- findReserveUtxos genesisUtxo
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                genesisUtxo

            unless (Map.isEmpty reserveUtxosAfterHandover)
              ( liftContract $ throwError $ error
                  "Reserve utxo still present after handover"
              )

            unless
              ( valueOf (fromAssetClass tokenKind) icsAfterTransfer ==
                  BigNum.fromInt numOfNonAdaTokens
              )
              ( liftContract $ throwError $ error
                  "Reserve tokens not transferred to illiquid circulation supply"
              )

testScenario7 :: TestnetTest
testScenario7 =
  test
    "Handover with ADA as reserve token"
    do
      withWallets initialDistribution \alice -> do
        withKeyWallet alice $ unliftApp do
          pkh <- getOwnPaymentPubKeyHash
          do
            genesisUtxo <- dummyInitialiseSidechain pkh

            let numOfAda = 3_000_000

            void $ initialiseReserveUtxo
              genesisUtxo
              immutableAdaSettings
              invalidMutableSettings
              (BigNum.fromInt numOfAda)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos genesisUtxo

            void $ handover
              genesisUtxo
              utxo

            reserveUtxosAfterHandover <- findReserveUtxos genesisUtxo
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                genesisUtxo

            unless (Map.isEmpty reserveUtxosAfterHandover)
              ( liftContract $ throwError $ error
                  "Reserve utxo still present after handover"
              )

            unless
              ( unwrap (getCoin icsAfterTransfer) ==
                  BigNum.fromInt numOfAda
              )
              ( liftContract $ throwError $ error
                  "Reserve tokens not transferred to illiquid circulation supply"
              )
