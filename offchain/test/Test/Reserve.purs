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
import Cardano.Types.Value (getCoin, valueOf)
import Cardano.Types.Value as Value
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Test.Testnet (withWallets)
import Contract.Transaction (TransactionOutput)
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
import TrustlessSidechain.Effects.Env (emptyEnv)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (unliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
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
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Asset (emptyAssetName)
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
    SidechainParams
dummyInitialiseSidechain pkh = do
  genesisUtxo <- Test.Utils.getOwnTransactionInput

  let
    sidechainParams =
      SidechainParams
        { chainId: BigInt.fromInt 1
        , genesisUtxo
        , thresholdNumerator: BigInt.fromInt 2
        , thresholdDenominator: BigInt.fromInt 3
        , governanceAuthority: Governance.mkGovernanceAuthority pkh
        }

  _ <- initTokensMint sidechainParams 1
  _ <- initNativeTokenMgmt sidechainParams 1

  pure sidechainParams

findIlliquidCirculationSupplyUtxos ::
  forall r.
  SidechainParams ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    UtxoMap
findIlliquidCirculationSupplyUtxos sidechainParams =
  Versioning.getVersionedValidatorAddress
    sidechainParams
    ( VersionOracle
        { version: BigNum.fromInt 1
        , scriptId: IlliquidCirculationSupplyValidator
        }
    )
    >>= utxosAt

mintNonAdaTokens ::
  forall r.
  Int.Int ->
  Run
    (EXCEPT OffchainError + LOG + TRANSACTION + r)
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
      withKeyWallet alice $ unliftApp emptyEnv do
        pkh <- getOwnPaymentPubKeyHash
        Test.Utils.withSingleMultiSig (unwrap pkh) $ do

          sidechainParams <- dummyInitialiseSidechain pkh

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

          void $ initialiseReserveUtxo
            sidechainParams
            immutableSettings
            invalidMutableSettings
            (BigNum.fromInt initialAmountOfNonAdaTokens)

          void $ depositToReserve
            sidechainParams
            (fromAssetClass tokenKind)
            (BigNum.fromInt depositAmountOfNonAdaTokens)

          maybeUtxo <- Map.toUnfoldable
            <$> findReserveUtxos sidechainParams

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
        withKeyWallet alice $ unliftApp emptyEnv do
          pkh <- getOwnPaymentPubKeyHash
          Test.Utils.withSingleMultiSig (unwrap pkh) $ do

            sidechainParams <- dummyInitialiseSidechain pkh

            void $ initialiseReserveUtxo
              sidechainParams
              immutableAdaSettings
              invalidMutableSettings
              (BigNum.fromInt 2_000_000)

            utxoBefore <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found" $
                (Map.toUnfoldable <$> findReserveUtxos sidechainParams)

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
              sidechainParams
              updatedMutableSettings
              utxoBefore

            utxoAfter <-
              Test.Utils.fromMaybeTestError "Utxo after update not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos sidechainParams

            let
              unwrappedDatum =
                snd
                  >>> extractReserveDatum
                  >>> map unwrap

              withUpdatedMutableSettings = _
                { mutableSettings = updatedMutableSettings }

            unless
              ( (withUpdatedMutableSettings <$> unwrappedDatum utxoBefore)
                  == unwrappedDatum utxoAfter
              )
              (liftContract $ throwError $ error "Update not sucessful")

testScenario5 :: TestnetTest
testScenario5 =
  test
    "Transfer to illiquid circulation supply with non-ADA as reserve token"
    do
      withWallets initialDistribution \alice -> do
        withKeyWallet alice $ unliftApp emptyEnv do
          pkh <- getOwnPaymentPubKeyHash
          Test.Utils.withSingleMultiSig (unwrap pkh) $ do
            sidechainParams <- dummyInitialiseSidechain pkh

            let
              numOfNonAdaTokens = 101
              numOfTransferTokens = 15
            tokenKind <- mintNonAdaTokens $ Int.fromInt numOfNonAdaTokens

            fakeVt <- alwaysPassingPolicy $ BigInt.fromInt 11

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
              sidechainParams
              immutableSettings
              mutableSettings
              (BigNum.fromInt numOfNonAdaTokens)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos sidechainParams

            void $ transferToIlliquidCirculationSupply
              sidechainParams
              numOfTransferTokens
              fakeVt
              utxo

            reserveAfterTransfer <- totalAssets <$> findReserveUtxos
              sidechainParams
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                sidechainParams

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
        withKeyWallet alice $ unliftApp emptyEnv do
          pkh <- getOwnPaymentPubKeyHash
          Test.Utils.withSingleMultiSig (unwrap pkh) $ do

            sidechainParams <- dummyInitialiseSidechain pkh

            fakeVt <- alwaysPassingPolicy $ BigInt.fromInt 11

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
              sidechainParams
              immutableAdaSettings
              mutableSettings
              (BigNum.fromInt numOfAda)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos sidechainParams

            void $ transferToIlliquidCirculationSupply
              sidechainParams
              numOfTransferred
              fakeVt
              utxo

            let amountOfReserveTokens t = unwrap $ getCoin t

            reserveAfterTransfer <- totalAssets <$> findReserveUtxos
              sidechainParams
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                sidechainParams

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
        withKeyWallet alice $ unliftApp emptyEnv do

          pkh <- getOwnPaymentPubKeyHash
          Test.Utils.withSingleMultiSig (unwrap pkh) $ do
            sidechainParams <- dummyInitialiseSidechain pkh

            let numOfNonAdaTokens = 101

            tokenKind <- mintNonAdaTokens $ Int.fromInt numOfNonAdaTokens

            let
              immutableSettings = ImmutableReserveSettings
                { t0: zero
                , tokenKind: fromAssetClass tokenKind
                }

            void $ initialiseReserveUtxo
              sidechainParams
              immutableSettings
              invalidMutableSettings
              (BigNum.fromInt numOfNonAdaTokens)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos sidechainParams

            void $ handover
              sidechainParams
              utxo

            reserveUtxosAfterHandover <- findReserveUtxos sidechainParams
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                sidechainParams

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
        withKeyWallet alice $ unliftApp emptyEnv do
          pkh <- getOwnPaymentPubKeyHash
          Test.Utils.withSingleMultiSig (unwrap pkh) $ do
            sidechainParams <- dummyInitialiseSidechain pkh

            let numOfAda = 3_000_000

            void $ initialiseReserveUtxo
              sidechainParams
              immutableAdaSettings
              invalidMutableSettings
              (BigNum.fromInt numOfAda)

            utxo <-
              Test.Utils.fromMaybeTestError "Utxo after initialization not found"
                $ Map.toUnfoldable
                <$> findReserveUtxos sidechainParams

            void $ handover
              sidechainParams
              utxo

            reserveUtxosAfterHandover <- findReserveUtxos sidechainParams
            icsAfterTransfer <- map totalAssets $
              findIlliquidCirculationSupplyUtxos
                sidechainParams

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
