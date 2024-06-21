module Test.Reserve
  ( tests
  ) where

import Contract.Prelude

import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts as Scripts
import Contract.Transaction (TransactionOutputWithRefScript)
import Contract.TxConstraints as TxConstraints
import Contract.Value
  ( Value
  , adaSymbol
  , adaToken
  , mkCurrencySymbol
  , mpsSymbol
  , scriptCurrencySymbol
  , valueOf
  )
import Contract.Value as Value
import Contract.Wallet as Wallet
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import Effect.Exception (error)
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Run (EFFECT, Run)
import Run.Except (EXCEPT)
import Test.AlwaysPassingScripts (alwaysPassingPolicy)
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain (InitSidechainParams(..), initSidechain)
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( findIlliquidCirculationSupplyUtxos
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
  ( ImmutableReserveSettings(..)
  , MutableReserveSettings(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (AssetClass)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Utils (insertVersionLookupsAndConstraints)
import Type.Row (type (+))

immutableAdaSettings ∷ ImmutableReserveSettings
immutableAdaSettings = ImmutableReserveSettings
  { t0: zero
  , tokenKind: adaSymbol /\ adaToken
  }

-- | `tests` aggregates all UpdateCommitteeHash the tests.
tests ∷ WrappedTests
tests = plutipGroup "Reserve" $ do
  testScenario1
  testScenario2
  testScenario3
  testScenario4
  testScenario5
  testScenario8
  testScenario6
  testScenario7

totalAssets ∷ ∀ f. Foldable f ⇒ f (TransactionOutputWithRefScript) → Value
totalAssets = foldMap
  $ unwrap
  >>> _.output
  >>> unwrap
  >>> _.amount

insertFakeGovernancePolicy ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
insertFakeGovernancePolicy sidechainParams =
  do
    governanceFakePolicy ← alwaysPassingPolicy $ BigInt.fromInt 10

    void
      $
        insertVersionLookupsAndConstraints sidechainParams 1
          (GovernancePolicy /\ governanceFakePolicy)
      >>= balanceSignAndSubmit "Insert governance policy"

invalidMutableSettings ∷ MutableReserveSettings
invalidMutableSettings = MutableReserveSettings
  { vFunctionTotalAccrued: unsafePartial $ fromJust $ mkCurrencySymbol mempty
  , incentiveAmount: BigInt.fromInt (-1)
  }

dummyInitialiseSidechain ∷
  ∀ r.
  Run
    (APP + EFFECT + CONTRACT + r)
    SidechainParams
dummyInitialiseSidechain = do
  genesisUtxo ← Test.Utils.getOwnTransactionInput

  pkh ← getOwnPaymentPubKeyHash
  initCommitteePrvKeys ←
    liftEffect
      $ sequence
      $ Array.replicate 1 generatePrivKey

  let
    initCommitteePubKeys = map toPubKeyUnsafe initCommitteePrvKeys
    initScParams = InitSidechainParams
      { initChainId: BigInt.fromInt 1
      , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
      , initUtxo: genesisUtxo
      , initAggregatedCommittee: toData $ aggregateKeys $ map unwrap
          initCommitteePubKeys
      , initSidechainEpoch: zero
      , initThresholdNumerator: BigInt.fromInt 2
      , initThresholdDenominator: BigInt.fromInt 3
      , initCandidatePermissionTokenMintInfo: Nothing
      , initATMSKind: ATMSPlainEcdsaSecp256k1
      , initGovernanceAuthority: Governance.mkGovernanceAuthority $ unwrap pkh
      }

  { sidechainParams } ← initSidechain initScParams 1

  pure sidechainParams

mintNonAdaTokens ∷
  ∀ r.
  BigInt →
  Run
    (EXCEPT OffchainError + LOG + TRANSACTION + r)
    AssetClass
mintNonAdaTokens numOfTokens = do
  policy ← alwaysPassingPolicy $ BigInt.fromInt 100

  let
    cs = unsafePartial $ fromJust $ scriptCurrencySymbol policy

    lookups = Lookups.mintingPolicy policy

    constraints =
      TxConstraints.mustMintValue
        (Value.singleton cs adaToken numOfTokens)

  void $ balanceSignAndSubmit
    "Reserve initialization transaction"
    { constraints, lookups }

  pure $ cs /\ adaToken

initialDistribution ∷ Array BigInt
initialDistribution =
  [ BigInt.fromInt 50_000_000
  , BigInt.fromInt 50_000_000
  , BigInt.fromInt 50_000_000
  , BigInt.fromInt 40_000_000
  , BigInt.fromInt 40_000_000
  ]

testScenario1 ∷ PlutipTest
testScenario1 =
  Mote.Monad.test "Successful reserve initialization with ADA as reserve token"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        initialiseReserveUtxo
          sidechainParams
          immutableAdaSettings
          invalidMutableSettings
          (BigInt.fromInt 100)

        utxoMap ← findReserveUtxos sidechainParams

        when (Map.isEmpty utxoMap)
          $ liftContract
          $ throwError
          $ error "Reserve utxo not found"

testScenario2 ∷ PlutipTest
testScenario2 =
  Mote.Monad.test
    "Successful reserve initialization with non-ADA as reserve token"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        let numOfNonAdaTokens = BigInt.fromInt 101

        tokenKind ← mintNonAdaTokens numOfNonAdaTokens

        let
          immutableSettings = ImmutableReserveSettings
            { t0: zero
            , tokenKind
            }

        initialiseReserveUtxo
          sidechainParams
          immutableSettings
          invalidMutableSettings
          numOfNonAdaTokens

        utxoMap ← findReserveUtxos sidechainParams

        when (Map.isEmpty utxoMap)
          $ liftContract
          $ throwError
          $ error "Reserve utxo not found"

testScenario3 ∷ PlutipTest
testScenario3 =
  Mote.Monad.test
    "Deposit more non-ADA to a reserve"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        let
          initialAmountOfNonAdaTokens = BigInt.fromInt 50
          depositAmountOfNonAdaTokens = BigInt.fromInt 51
          numOfNonAdaTokens =
            initialAmountOfNonAdaTokens + depositAmountOfNonAdaTokens

        tokenKind ← mintNonAdaTokens numOfNonAdaTokens

        let
          immutableSettings = ImmutableReserveSettings
            { t0: zero
            , tokenKind
            }

        initialiseReserveUtxo
          sidechainParams
          immutableSettings
          invalidMutableSettings
          initialAmountOfNonAdaTokens

        depositToReserve
          sidechainParams
          tokenKind
          depositAmountOfNonAdaTokens

        maybeUtxo ← Map.toUnfoldable
          <$> findReserveUtxos sidechainParams

        let
          extractValue = snd >>> unwrap >>> _.output >>> unwrap >>> _.amount
          isExpectedAmount = \v → valueOf v (fst tokenKind) (snd tokenKind)

        unless
          ( Just numOfNonAdaTokens ==
              (extractValue >>> isExpectedAmount <$> maybeUtxo)
          )
          (liftContract $ throwError $ error "Deposit not sucessful")

testScenario4 ∷ PlutipTest
testScenario4 =
  Mote.Monad.test
    "Update reserve utxo mutable settings"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice)
        do
          sidechainParams ← dummyInitialiseSidechain

          insertFakeGovernancePolicy sidechainParams

          initialiseReserveUtxo
            sidechainParams
            immutableAdaSettings
            invalidMutableSettings
            (BigInt.fromInt 2_000_000)

          utxoBefore ←
            Test.Utils.fromMaybeTestError "Utxo after initialization not found" $
              (Map.toUnfoldable <$> findReserveUtxos sidechainParams)

          let
            updatedMutableSettings = MutableReserveSettings
              { vFunctionTotalAccrued:
                  unsafePartial
                    $ fromJust
                    $ mkCurrencySymbol
                    $ hexToByteArrayUnsafe
                        "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
              , incentiveAmount: BigInt.fromInt 20
              }

          updateReserveUtxo
            sidechainParams
            updatedMutableSettings
            utxoBefore

          utxoAfter ←
            Test.Utils.fromMaybeTestError "Utxo after update not found"
              $ Map.toUnfoldable
              <$> findReserveUtxos sidechainParams

          let
            unwrappedDatum =
              snd
                >>> unwrap
                >>> _.output
                >>> extractReserveDatum
                >>> map unwrap

            withUpdatedMutableSettings = _
              { mutableSettings = updatedMutableSettings }

          unless
            ( (withUpdatedMutableSettings <$> unwrappedDatum utxoBefore)
                == unwrappedDatum utxoAfter
            )
            (liftContract $ throwError $ error "Update not sucessful")

testScenario5 ∷ PlutipTest
testScenario5 =
  Mote.Monad.test
    "Transfer to illiquid circulation supply with non-ADA as reserve token"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        let
          numOfNonAdaTokens = BigInt.fromInt 101
          numOfTransferTokens = BigInt.fromInt 15
        tokenKind ← mintNonAdaTokens numOfNonAdaTokens

        fakeVt ← alwaysPassingPolicy $ BigInt.fromInt 11

        let
          incentiveAmount = BigInt.fromInt 1

          mutableSettings = MutableReserveSettings
            { vFunctionTotalAccrued:
                unsafePartial
                  $ fromJust
                  $ mpsSymbol
                  $ Scripts.mintingPolicyHash
                  $ fakeVt
            , incentiveAmount
            }

          immutableSettings = ImmutableReserveSettings
            { t0: zero
            , tokenKind
            }

        initialiseReserveUtxo
          sidechainParams
          immutableSettings
          mutableSettings
          numOfNonAdaTokens

        utxo ← Test.Utils.fromMaybeTestError "Utxo after initialization not found"
          $ Map.toUnfoldable
          <$> findReserveUtxos sidechainParams

        transferToIlliquidCirculationSupply
          sidechainParams
          numOfTransferTokens
          fakeVt
          utxo

        let amountOfReserveTokens t = uncurry (valueOf t) tokenKind

        reserveAfterTransfer ← totalAssets <$> findReserveUtxos sidechainParams
        icsAfterTransfer ← map totalAssets $ findIlliquidCirculationSupplyUtxos
          sidechainParams

        unless
          ( amountOfReserveTokens reserveAfterTransfer ==
              (numOfNonAdaTokens - numOfTransferTokens)
          )
          ( liftContract $ throwError $ error
              "Incorrect number of reserve tokens in reserve after transfer"
          )

        unless
          ( amountOfReserveTokens icsAfterTransfer ==
              (numOfTransferTokens - incentiveAmount)
          )
          ( liftContract $ throwError $ error
              "Incorrect number of reserve tokens in ICS after transfer"
          )

        uncurry Test.Utils.assertIHaveOutputWithAsset tokenKind

        pure unit

testScenario8 ∷ PlutipTest
testScenario8 =
  Mote.Monad.test
    "Transfer to illiquid circulation supply with ADA as reserve token"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        fakeVt ← alwaysPassingPolicy $ BigInt.fromInt 11

        let
          numOfAda = BigInt.fromInt 5_000_000
          numOfTransferred = BigInt.fromInt 3_000_000
          incentiveAmount = BigInt.fromInt 0
          mutableSettings = MutableReserveSettings
            { vFunctionTotalAccrued:
                unsafePartial
                  $ fromJust
                  $ mpsSymbol
                  $ Scripts.mintingPolicyHash
                  $ fakeVt
            , incentiveAmount
            }

        initialiseReserveUtxo
          sidechainParams
          immutableAdaSettings
          mutableSettings
          numOfAda

        utxo ← Test.Utils.fromMaybeTestError "Utxo after initialization not found"
          $ Map.toUnfoldable
          <$> findReserveUtxos sidechainParams

        transferToIlliquidCirculationSupply
          sidechainParams
          numOfTransferred
          fakeVt
          utxo

        let amountOfReserveTokens t = uncurry (valueOf t) (adaSymbol /\ adaToken)

        reserveAfterTransfer ← totalAssets <$> findReserveUtxos sidechainParams
        icsAfterTransfer ← map totalAssets $ findIlliquidCirculationSupplyUtxos
          sidechainParams

        unless
          ( amountOfReserveTokens reserveAfterTransfer ==
              (numOfAda - numOfTransferred)
          )
          ( liftContract $ throwError $ error
              "Incorrect number of reserve tokens in reserve after transfer"
          )

        unless
          ( amountOfReserveTokens icsAfterTransfer == numOfTransferred
          )
          ( liftContract $ throwError $ error
              "Incorrect number of reserve tokens in ICS after transfer"
          )

        pure unit

testScenario6 ∷ PlutipTest
testScenario6 =
  Mote.Monad.test
    "Handover with non-ADA as reserve token"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        let numOfNonAdaTokens = BigInt.fromInt 101

        tokenKind ← mintNonAdaTokens numOfNonAdaTokens

        let
          immutableSettings = ImmutableReserveSettings
            { t0: zero
            , tokenKind
            }

        initialiseReserveUtxo
          sidechainParams
          immutableSettings
          invalidMutableSettings
          numOfNonAdaTokens

        utxo ← Test.Utils.fromMaybeTestError "Utxo after initialization not found"
          $ Map.toUnfoldable
          <$> findReserveUtxos sidechainParams

        handover
          sidechainParams
          utxo

        reserveUtxosAfterHandover ← findReserveUtxos sidechainParams
        icsAfterTransfer ← map totalAssets $ findIlliquidCirculationSupplyUtxos
          sidechainParams

        unless (Map.isEmpty reserveUtxosAfterHandover)
          ( liftContract $ throwError $ error
              "Reserve utxo still present after handover"
          )

        unless
          ( valueOf icsAfterTransfer (fst tokenKind) (snd tokenKind) ==
              numOfNonAdaTokens
          )
          ( liftContract $ throwError $ error
              "Reserve tokens not transferred to illiquid circulation supply"
          )

testScenario7 ∷ PlutipTest
testScenario7 =
  Mote.Monad.test
    "Handover with ADA as reserve token"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain

        insertFakeGovernancePolicy sidechainParams

        let numOfAda = BigInt.fromInt 3_000_000

        initialiseReserveUtxo
          sidechainParams
          immutableAdaSettings
          invalidMutableSettings
          numOfAda

        utxo ← Test.Utils.fromMaybeTestError "Utxo after initialization not found"
          $ Map.toUnfoldable
          <$> findReserveUtxos sidechainParams

        handover
          sidechainParams
          utxo

        reserveUtxosAfterHandover ← findReserveUtxos sidechainParams
        icsAfterTransfer ← map totalAssets $ findIlliquidCirculationSupplyUtxos
          sidechainParams

        unless (Map.isEmpty reserveUtxosAfterHandover)
          ( liftContract $ throwError $ error
              "Reserve utxo still present after handover"
          )

        unless
          ( valueOf icsAfterTransfer adaSymbol adaToken ==
              numOfAda
          )
          ( liftContract $ throwError $ error
              "Reserve tokens not transferred to illiquid circulation supply"
          )
