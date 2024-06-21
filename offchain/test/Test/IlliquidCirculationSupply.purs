module Test.IlliquidCirculationSupply
  ( tests
  ) where

import Contract.Prelude

import Contract.PlutusData (Datum(..), toData)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, MintingPolicyHash)
import Contract.Scripts (mintingPolicyHash, validatorHash) as Scripts
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as TxConstraints
import Contract.Value (adaSymbol, adaToken, valueOf)
import Contract.Value (scriptCurrencySymbol) as Scripts
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
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain (InitSidechainParams(..), initSidechain)
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( depositMoreToSupply
  , findIlliquidCirculationSupplyUtxos
  , illiquidCirculationSupplyValidator
  , withdrawFromSupply
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
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | `tests` aggregates all UpdateCommitteeHash the tests.
tests ∷ WrappedTests
tests = plutipGroup "IlliquidCirculationSupply" $ do
  testScenario1
  testScenario2

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
    cs = unsafePartial $ fromJust $ Scripts.scriptCurrencySymbol policy

    lookups = Lookups.mintingPolicy policy

    constraints =
      TxConstraints.mustMintValue
        (Value.singleton cs adaToken numOfTokens)

  void $ balanceSignAndSubmit
    "mintNonAdaTokens transaction"
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

    illiquidCirculationSupplyValidator' ← Scripts.validatorHash <$>
      illiquidCirculationSupplyValidator
        versionOracleConfig

    let
      lookups ∷ Lookups.ScriptLookups Void
      lookups =
        mempty

      constraints =
        TxConstraints.mustPayToScript
          illiquidCirculationSupplyValidator'
          (Datum $ toData unit)
          DatumInline
          (Value.singleton adaSymbol adaToken (BigInt.fromInt 1))

    void $ balanceSignAndSubmit
      "ICS initialization transaction"
      { constraints, lookups }

mkIcsFakePolicy ∷
  ∀ r.
  Run
    (EXCEPT OffchainError + r)
    MintingPolicy
mkIcsFakePolicy = alwaysPassingPolicy $ BigInt.fromInt 43

insertFakeIcsWithdrawalPolicy ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    MintingPolicyHash
insertFakeIcsWithdrawalPolicy sidechainParams =
  do
    icsFakePolicy ← mkIcsFakePolicy

    void
      $
        insertVersionLookupsAndConstraints sidechainParams 1
          (IlliquidCirculationSupplyWithdrawalPolicy /\ icsFakePolicy)
      >>= balanceSignAndSubmit
        "Insert illiquid circulation withdrawal minting policy"

    pure $ Scripts.mintingPolicyHash icsFakePolicy

findICSUtxo ∷
  ∀ r.
  SidechainParams →
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
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
        sidechainParams ← dummyInitialiseSidechain

        initialiseICSUtxo sidechainParams
        utxo ← findICSUtxo sidechainParams

        let
          depositAmountOfNonAdaTokens = BigInt.fromInt 51

        tokenKind ← mintNonAdaTokens depositAmountOfNonAdaTokens

        let
          addedValue = Value.singleton (fst tokenKind) (snd tokenKind)
            depositAmountOfNonAdaTokens

        depositMoreToSupply
          sidechainParams
          addedValue
          utxo

        maybeUtxo ← Map.toUnfoldable
          <$> findIlliquidCirculationSupplyUtxos sidechainParams

        let
          extractValue = snd >>> unwrap >>> _.output >>> unwrap >>> _.amount
          isExpectedAmount = \v → valueOf v (fst tokenKind) (snd tokenKind)

        unless
          ( Just depositAmountOfNonAdaTokens ==
              (extractValue >>> isExpectedAmount <$> maybeUtxo)
          )
          (liftContract $ throwError $ error "Deposit not sucessful")

testScenario2 ∷ PlutipTest
testScenario2 =
  Mote.Monad.test
    "Withdraw from ICS"
    $ Test.PlutipTest.mkPlutipConfigTest initialDistribution
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        sidechainParams ← dummyInitialiseSidechain
        mintingPolicyHash ← insertFakeIcsWithdrawalPolicy sidechainParams

        initialiseICSUtxo sidechainParams
        utxo1 ← findICSUtxo sidechainParams

        let
          depositAmountOfNonAdaTokens = BigInt.fromInt 51
          withdrawAmountOfNonAdaTokens = BigInt.fromInt 42
          finalAmountOfNonAdaTokens = depositAmountOfNonAdaTokens -
            withdrawAmountOfNonAdaTokens

        tokenKind ← mintNonAdaTokens depositAmountOfNonAdaTokens

        let
          addedValue = Value.singleton (fst tokenKind) (snd tokenKind)
            depositAmountOfNonAdaTokens

        depositMoreToSupply
          sidechainParams
          addedValue
          utxo1

        utxo2 ← findICSUtxo sidechainParams

        let
          withdrawnValue = Value.singleton (fst tokenKind) (snd tokenKind)
            withdrawAmountOfNonAdaTokens

        withdrawFromSupply
          sidechainParams
          mintingPolicyHash
          withdrawnValue
          utxo2

        maybeUtxo ← Map.toUnfoldable
          <$> findIlliquidCirculationSupplyUtxos sidechainParams

        let
          extractValue = snd >>> unwrap >>> _.output >>> unwrap >>> _.amount
          isExpectedAmount = \v → valueOf v (fst tokenKind) (snd tokenKind)

        unless
          ( Just finalAmountOfNonAdaTokens ==
              (extractValue >>> isExpectedAmount <$> maybeUtxo)
          )
          (liftContract $ throwError $ error "Withdrawal not sucessful")
