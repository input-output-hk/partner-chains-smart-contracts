module Test.IlliquidCirculationSupply
  ( tests
  ) where

import Contract.Prelude
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Mint as Mint
import Cardano.Types.Int as Int
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as TxConstraints
import Cardano.Types.Value (valueOf)
import Contract.Utxos (UtxoMap)
import Contract.Value as Value
import Contract.Wallet as Wallet
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import JS.BigInt as BigInt
import Data.Map as Map
import Effect.Exception (error)
import Mote.Monad as Mote.Monad
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
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Effects.Env (Env, READER)
import TrustlessSidechain.Error (OffchainError(NotFoundUtxo))
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , initSidechain
  )
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( depositMoreToSupply
  , illiquidCirculationSupplyValidator
  , withdrawFromSupply
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.Asset (fromAssetClass)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Crypto
  ( aggregateKeys
  , generatePrivKey
  , toPubKeyUnsafe
  )
import TrustlessSidechain.Utils.Asset (emptyAssetName, singletonFromAsset)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( IlliquidCirculationSupplyValidator
      , IlliquidCirculationSupplyWithdrawalPolicy
      )
  )
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils (insertVersionLookupsAndConstraints)
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | `tests` aggregates all UpdateCommitteeHash the tests.
tests ∷ WrappedTests
tests = plutipGroup "IlliquidCirculationSupply" $ do
  testScenario1
  testScenario2

dummyInitialiseSidechain ∷
  ∀ r.
  PaymentPubKeyHash ->
  Run
    (APP + EFFECT + CONTRACT + r)
    SidechainParams
dummyInitialiseSidechain pkh = do
  genesisUtxo ← Test.Utils.getOwnTransactionInput

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
      , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
      }

  { sidechainParams } ← initSidechain initScParams 1

  pure sidechainParams

mintNonAdaTokens ∷
  ∀ r.
  Int →
  Run
    (EXCEPT OffchainError + LOG + TRANSACTION + r)
    AssetClass
mintNonAdaTokens numOfTokens = do
  policy ← alwaysPassingPolicy $ BigInt.fromInt 100

  let
    cs = PlutusScript.hash policy

    lookups = Lookups.plutusMintingPolicy policy

    constraints =
      TxConstraints.mustMintValue
        (Mint.singleton cs emptyAssetName $ Int.fromInt numOfTokens)

  void $ balanceSignAndSubmit
    "mintNonAdaTokens transaction"
    { constraints, lookups }

  pure $ AssetClass cs emptyAssetName

initialDistribution ∷ Array BigNum
initialDistribution =
  [ BigNum.fromInt 50_000_000
  , BigNum.fromInt 50_000_000
  , BigNum.fromInt 50_000_000
  , BigNum.fromInt 40_000_000
  , BigNum.fromInt 40_000_000
  ]

initialiseICSUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
initialiseICSUtxo
  sidechainParams =
  do
    versionOracleConfig ← Versioning.getVersionOracleConfig sidechainParams

    illiquidCirculationSupplyValidator' ← PlutusScript.hash <$>
      illiquidCirculationSupplyValidator
        versionOracleConfig

    let
      lookups ∷ Lookups.ScriptLookups
      lookups =
        mempty

      constraints =
        TxConstraints.mustPayToScript
          illiquidCirculationSupplyValidator'
          (toData unit)
          DatumInline
          (Value.mkValue (wrap (BigNum.fromInt 1)) MultiAsset.empty)

    void $ balanceSignAndSubmit
      "ICS initialization transaction"
      { constraints, lookups }

mkIcsFakePolicy ∷
  ∀ r.
  Run
    (EXCEPT OffchainError + r)
    PlutusScript
mkIcsFakePolicy = alwaysPassingPolicy $ BigInt.fromInt 43

insertFakeIcsWithdrawalPolicy ∷
  ∀ r.
  SidechainParams →
  Run
    (READER Env + EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    ScriptHash
insertFakeIcsWithdrawalPolicy sidechainParams =
  do
    icsFakePolicy ← mkIcsFakePolicy

    void
      $
        insertVersionLookupsAndConstraints sidechainParams 1
          (IlliquidCirculationSupplyWithdrawalPolicy /\ icsFakePolicy)
      >>= balanceSignAndSubmit
        "Insert illiquid circulation withdrawal minting policy"

    pure $ PlutusScript.hash icsFakePolicy

findIlliquidCirculationSupplyUtxos ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    UtxoMap
findIlliquidCirculationSupplyUtxos sidechainParams =
    Versioning.getVersionedValidatorAddress
    sidechainParams
    (VersionOracle { version: BigNum.fromInt 1
                   , scriptId: IlliquidCirculationSupplyValidator })
      >>= utxosAt

findICSUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    (TransactionInput /\ TransactionOutput)
findICSUtxo
  sidechainParams =
  fromMaybeThrow
    (NotFoundUtxo "IlliquidCirculationSupply UTxO not found")
    $
      ( Map.toUnfoldable <$> findIlliquidCirculationSupplyUtxos sidechainParams
      )

testScenario1 ∷ PlutipTest
testScenario1 =
  Mote.Monad.test
    "Deposit to ICS"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh <- getOwnPaymentPubKeyHash
        Test.Utils.withSingleMultiSig (unwrap pkh) $ do

          sidechainParams ← dummyInitialiseSidechain pkh

          initialiseICSUtxo sidechainParams

          utxo ← findICSUtxo sidechainParams

          let
            depositAmountOfNonAdaTokens = 51

          tokenKind ← mintNonAdaTokens depositAmountOfNonAdaTokens

          let
            addedValue = singletonFromAsset (fromAssetClass tokenKind)
              $ BigNum.fromInt depositAmountOfNonAdaTokens

          depositMoreToSupply
            sidechainParams
            addedValue
            utxo

          maybeUtxo ← Map.toUnfoldable
            <$> findIlliquidCirculationSupplyUtxos sidechainParams

          let
            extractValue = snd >>> unwrap >>> _.amount
            isExpectedAmount = valueOf (fromAssetClass tokenKind)

          unless
            ( Just (BigNum.fromInt depositAmountOfNonAdaTokens) ==
                (extractValue >>> isExpectedAmount <$> maybeUtxo)
            )
            (liftContract $ throwError $ error "Deposit not sucessful")

testScenario2 ∷ PlutipTest
testScenario2 =
  Mote.Monad.test
    "Withdraw from ICS"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        pkh <- getOwnPaymentPubKeyHash
        Test.Utils.withSingleMultiSig (unwrap pkh) $ do

          sidechainParams ← dummyInitialiseSidechain pkh
          mintingPolicyHash ← insertFakeIcsWithdrawalPolicy sidechainParams

          initialiseICSUtxo sidechainParams
          utxo1 ← findICSUtxo sidechainParams

          let
            depositAmountOfNonAdaTokens = 51
            withdrawAmountOfNonAdaTokens = 42
            finalAmountOfNonAdaTokens = depositAmountOfNonAdaTokens -
              withdrawAmountOfNonAdaTokens

          tokenKind ← mintNonAdaTokens depositAmountOfNonAdaTokens

          let
            addedValue = singletonFromAsset (fromAssetClass tokenKind)
              $ BigNum.fromInt depositAmountOfNonAdaTokens

          depositMoreToSupply
            sidechainParams
            addedValue
            utxo1

          utxo2 ← findICSUtxo sidechainParams

          let
            withdrawnValue = singletonFromAsset (fromAssetClass tokenKind)
              $ BigNum.fromInt withdrawAmountOfNonAdaTokens

          withdrawFromSupply
            sidechainParams
            mintingPolicyHash
            withdrawnValue
            utxo2

          maybeUtxo ← Map.toUnfoldable
            <$> findIlliquidCirculationSupplyUtxos sidechainParams

          let
            extractValue = snd >>> unwrap >>> _.amount
            isExpectedAmount = valueOf (fromAssetClass tokenKind)

          unless
            ( Just (BigNum.fromInt finalAmountOfNonAdaTokens) ==
                (extractValue >>> isExpectedAmount <$> maybeUtxo)
            )
            (liftContract $ throwError $ error "Withdrawal not sucessful")
