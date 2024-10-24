module Test.IlliquidCirculationSupply
  ( suite
  ) where

import Contract.Prelude

import Cardano.Types (UtxoMap)
import Cardano.Types.Asset (fromAssetClass)
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.Value (valueOf)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Test.Testnet (withWallets)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as TxConstraints
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (throwError)
import Data.Map as Map
import Effect.Exception (error)
import JS.BigInt as BigInt
import Mote.Monad (group, test)
import Run (EFFECT, Run)
import Run.Except (EXCEPT)
import Test.AlwaysPassingScripts (alwaysPassingPolicy)
import Test.Utils (TestnetTest)
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Contract (CONTRACT, liftContract)
import TrustlessSidechain.Effects.Env (Env, READER, emptyEnv)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (unliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( depositMoreToSupply
  , illiquidCirculationSupplyValidator
  , withdrawFromSupply
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Asset (emptyAssetName, singletonFromAsset)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( IlliquidCirculationSupplyValidator
      , IlliquidCirculationSupplyWithdrawalPolicy
      )
  )
import TrustlessSidechain.Versioning.Types (VersionOracle(VersionOracle))
import TrustlessSidechain.Versioning.Utils (insertVersionLookupsAndConstraints)
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

suite :: TestnetTest
suite = group "IlliquidCirculationSupply" do
  testScenario

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
        { chainId: BigInt.fromInt 69_420
        , genesisUtxo
        , thresholdNumerator: BigInt.fromInt 2
        , thresholdDenominator: BigInt.fromInt 3
        , governanceAuthority: Governance.mkGovernanceAuthority pkh
        }

  _ <- initTokensMint sidechainParams 1
  _ <- initNativeTokenMgmt sidechainParams 1

  pure sidechainParams

mintNonAdaTokens ::
  forall r.
  Int ->
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
        (Mint.singleton cs emptyAssetName $ Int.fromInt numOfTokens)

  void $ balanceSignAndSubmit
    "mintNonAdaTokens transaction"
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

initialiseICSUtxo ::
  forall r.
  SidechainParams ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
initialiseICSUtxo
  sidechainParams =
  do
    versionOracleConfig <- Versioning.getVersionOracleConfig sidechainParams

    illiquidCirculationSupplyValidator' <- PlutusScript.hash <$>
      illiquidCirculationSupplyValidator
        versionOracleConfig

    let
      lookups :: Lookups.ScriptLookups
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

mkIcsFakePolicy ::
  forall r.
  Run
    (EXCEPT OffchainError + r)
    PlutusScript
mkIcsFakePolicy = alwaysPassingPolicy $ BigInt.fromInt 43

insertFakeIcsWithdrawalPolicy ::
  forall r.
  SidechainParams ->
  Run
    (READER Env + EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    ScriptHash
insertFakeIcsWithdrawalPolicy sidechainParams =
  do
    icsFakePolicy <- mkIcsFakePolicy

    void
      $
        insertVersionLookupsAndConstraints sidechainParams 1
          (IlliquidCirculationSupplyWithdrawalPolicy /\ icsFakePolicy)
      >>= balanceSignAndSubmit
        "Insert illiquid circulation withdrawal minting policy"

    pure $ PlutusScript.hash icsFakePolicy

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

findICSUtxo ::
  forall r.
  SidechainParams ->
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

testScenario :: TestnetTest
testScenario =
  test "Withdraw from ICS" do
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp emptyEnv do
        pkh <- getOwnPaymentPubKeyHash
        Test.Utils.withSingleMultiSig (unwrap pkh) $ do

          sidechainParams <- dummyInitialiseSidechain pkh
          mintingPolicyHash <- insertFakeIcsWithdrawalPolicy sidechainParams

          initialiseICSUtxo sidechainParams
          utxo1 <- findICSUtxo sidechainParams

          let
            depositAmountOfNonAdaTokens = 51
            withdrawAmountOfNonAdaTokens = 42
            finalAmountOfNonAdaTokens = depositAmountOfNonAdaTokens -
              withdrawAmountOfNonAdaTokens

          tokenKind <- mintNonAdaTokens depositAmountOfNonAdaTokens

          let
            addedValue = singletonFromAsset (fromAssetClass tokenKind)
              $ BigNum.fromInt depositAmountOfNonAdaTokens

          depositMoreToSupply
            sidechainParams
            addedValue
            utxo1

          utxo2 <- findICSUtxo sidechainParams

          let
            withdrawnValue = singletonFromAsset (fromAssetClass tokenKind)
              $ BigNum.fromInt withdrawAmountOfNonAdaTokens

          withdrawFromSupply
            sidechainParams
            mintingPolicyHash
            withdrawnValue
            utxo2

          maybeUtxo <- Map.toUnfoldable
            <$> findIlliquidCirculationSupplyUtxos sidechainParams

          let
            extractValue = snd >>> unwrap >>> _.amount
            isExpectedAmount = valueOf (fromAssetClass tokenKind)

          unless
            ( Just (BigNum.fromInt finalAmountOfNonAdaTokens) ==
                (extractValue >>> isExpectedAmount <$> maybeUtxo)
            )
            (liftContract $ throwError $ error "Withdrawal not sucessful")
