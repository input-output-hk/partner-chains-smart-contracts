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
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (unliftApp)
import TrustlessSidechain.Effects.Time (TIME)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.InitSidechain.Governance (initGovernance)
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( depositMoreToSupply
  , illiquidCirculationSupplyValidator
  , withdrawFromSupply
  )
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Asset (emptyAssetName, singletonFromAsset)
import TrustlessSidechain.Utils.Data
  ( VersionedGenericDatum(VersionedGenericDatum)
  )
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
    TransactionInput
dummyInitialiseSidechain pkh = do
  genesisUtxo <- Test.Utils.getOwnTransactionInput

  _ <- initGovernance genesisUtxo pkh
  _ <- initNativeTokenMgmt genesisUtxo

  pure genesisUtxo

mintNonAdaTokens ::
  forall r.
  Int ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
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
  TransactionInput ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    Unit
initialiseICSUtxo
  genesisUtxo =
  do
    versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo

    illiquidCirculationSupplyValidator' <- PlutusScript.hash <$>
      illiquidCirculationSupplyValidator
        versionOracleConfig

    let
      datum = toData $ VersionedGenericDatum
        { datum: unit
        , builtinData: toData unit
        , version: BigInt.fromInt 0
        }

      lookups :: Lookups.ScriptLookups
      lookups =
        mempty

      constraints =
        TxConstraints.mustPayToScript
          illiquidCirculationSupplyValidator'
          datum
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
  TransactionInput ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + TIME + r)
    ScriptHash
insertFakeIcsWithdrawalPolicy genesisUtxo =
  do
    icsFakePolicy <- mkIcsFakePolicy

    void
      $
        insertVersionLookupsAndConstraints genesisUtxo
          (IlliquidCirculationSupplyWithdrawalPolicy /\ icsFakePolicy)
      >>= balanceSignAndSubmit
        "Insert illiquid circulation withdrawal minting policy"

    pure $ PlutusScript.hash icsFakePolicy

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

findICSUtxo ::
  forall r.
  TransactionInput ->
  Run
    (EXCEPT OffchainError + WALLET + LOG + TRANSACTION + r)
    (TransactionInput /\ TransactionOutput)
findICSUtxo
  genesisUtxo =
  fromMaybeThrow
    (NotFoundUtxo "IlliquidCirculationSupply UTxO not found")
    $
      ( Map.toUnfoldable <$> findIlliquidCirculationSupplyUtxos genesisUtxo
      )

testScenario :: TestnetTest
testScenario =
  test "Withdraw from ICS" do
    withWallets initialDistribution \alice -> do
      withKeyWallet alice $ unliftApp do
        pkh <- getOwnPaymentPubKeyHash
        do

          genesisUtxo <- dummyInitialiseSidechain pkh
          mintingPolicyHash <- insertFakeIcsWithdrawalPolicy genesisUtxo

          initialiseICSUtxo genesisUtxo
          utxo1 <- findICSUtxo genesisUtxo

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
            genesisUtxo
            addedValue
            utxo1

          utxo2 <- findICSUtxo genesisUtxo

          let
            withdrawnValue = singletonFromAsset (fromAssetClass tokenKind)
              $ BigNum.fromInt withdrawAmountOfNonAdaTokens

          withdrawFromSupply
            genesisUtxo
            mintingPolicyHash
            withdrawnValue
            utxo2

          maybeUtxo <- Map.toUnfoldable
            <$> findIlliquidCirculationSupplyUtxos genesisUtxo

          let
            extractValue = snd >>> unwrap >>> _.amount
            isExpectedAmount = valueOf (fromAssetClass tokenKind)

          unless
            ( Just (BigNum.fromInt finalAmountOfNonAdaTokens) ==
                (extractValue >>> isExpectedAmount <$> maybeUtxo)
            )
            (liftContract $ throwError $ error "Withdrawal not sucessful")
