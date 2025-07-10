{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.TrustlessSidechain.Transactions where

import Cardano.Api qualified as CA
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Hashes qualified as Hash
import Convex.BuildTx qualified as BuildTx
import Convex.CoinSelection qualified as CoinSelection
import Convex.MockChain.CoinSelection qualified as MockChainCoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils qualified as MockChainUtils
import Convex.PlutusTx qualified as CPlutusTx
import Convex.Utils qualified as Utils
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.String (fromString)
import GHC.Exts (IsList (..), fromList)
import GHC.Num qualified as Num
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 qualified as PLAV1
import PlutusLedgerApi.V1.Data.Value qualified as Value
import PlutusTx qualified as PlutusTx
import PlutusTx.Builtins qualified as Builtins
import Test.HUnit qualified as HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TrustlessSidechain.Versioning qualified as Versioning
import Prelude

-- import Cardano.Api (fromLedgerUTxO)

-- | Transaction tests
--
-- @since 0.1
tests :: TestTree
tests =
  testGroup
    "Transaction tests"
    [ testCase "spendPublicKeyOutput" spendPublicKeyOutput
    , testCase "initializeGovernance" initializeGovernance
    ]

spendPublicKeyOutput :: HUnit.Assertion
spendPublicKeyOutput = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  let tx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue 10_000_000))
  MockChainCoinSelection.balanceAndSubmit mempty Wallet.w1 tx CoinSelection.TrailingChange []

appliedVersioningValidator :: C.TxIn -> C.PlutusScript C.PlutusScriptV2
appliedVersioningValidator (C.TxIn (CA.TxId genesisTxHash) (CA.TxIx genesisTxIx)) = CPlutusTx.compiledCodeToScript compiled
  where
    eCompiled = ($$(PlutusTx.compile [||Versioning.mkVersionOracleValidatorUntyped||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 txOutRefBuiltin)
    cborHash = Hash.originalBytes genesisTxHash
    txOutRefBuiltin = Builtins.mkConstr 0 [Builtins.mkB $ Builtins.toBuiltin cborHash, Builtins.mkI $ Num.integerFromWord genesisTxIx]
    compiled = case eCompiled of
      Left err -> error $ "Failed to compile versioning validator: " ++ show err
      Right x -> x

versioningValidatorHash :: C.TxIn -> C.ScriptHash
versioningValidatorHash = C.hashScript . C.PlutusScript C.PlutusScriptV2 . appliedVersioningValidator

appliedVersioningPolicy :: C.TxIn -> C.PlutusScript C.PlutusScriptV2
appliedVersioningPolicy c@(C.TxIn (CA.TxId genesisTxHash) (CA.TxIx genesisTxIx)) = CPlutusTx.compiledCodeToScript compiled2
  where
    validatorAddress = PlutusTx.liftCode plcVersion100 $ Builtins.mkConstr 0 [Builtins.mkConstr 1 [Builtins.mkB $ Builtins.toBuiltin $ C.serialiseToRawBytes $ versioningValidatorHash c], Builtins.mkConstr 1 []]
    cborHash = Hash.originalBytes genesisTxHash
    txOutRefBuiltin = PlutusTx.liftCode plcVersion100 $ Builtins.mkConstr 0 [Builtins.mkConstr 0 [Builtins.mkB $ Builtins.toBuiltin cborHash], Builtins.mkI $ Num.integerFromWord genesisTxIx]

    compiled = ($$(PlutusTx.compile [||Versioning.mkVersionOraclePolicyUntyped||]) `PlutusTx.unsafeApplyCode` txOutRefBuiltin)
    -- compiled = case eCompiled1 of
    --   Left err -> error $ "Failed to compile versioning validator: " ++ show err
    --   Right x -> x

    eCompiled2 = (compiled `PlutusTx.applyCode` validatorAddress)
    compiled2 = case eCompiled2 of
      Left err -> error $ "Failed to compile versioning validator: " ++ show err
      Right x -> x

versioningPolicyHash :: C.TxIn -> C.ScriptHash
versioningPolicyHash = C.hashScript . C.PlutusScript C.PlutusScriptV2 . appliedVersioningPolicy

versioningScriptAddress :: C.TxIn -> C.AddressInEra C.ConwayEra
versioningScriptAddress genesisUtxo = Utils.scriptAddress Defaults.networkId $ appliedVersioningValidator genesisUtxo

initializeGovernance :: HUnit.Assertion
initializeGovernance = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  let tx_0 = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue 10_000_000))
  tx_0_0 <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 tx_0 CoinSelection.TrailingChange []
  let txId = C.getTxId $ C.getTxBody tx_0_0
  -- utxo <- (head . Map.keys . CA.unUTxO) <$> Class.getUtxo
  -- let genesisUtxo = fromLedgerUTxO $ head genesisUtxos

  let genesisUtxo = C.TxIn txId (C.TxIx 0)
  let
    value :: C.Value
    value =
      fromList
        [ (CA.AssetId (CA.PolicyId (versioningPolicyHash genesisUtxo)) (CA.AssetName "Version oracle"), CA.Quantity 1)
        , (CA.AdaAssetId, CA.Quantity 10000000)
        ]

  let datum = Versioning.VersionOracleDatum (Versioning.VersionOracle 32) (Value.currencySymbol $ C.serialiseToRawBytes (versioningPolicyHash genesisUtxo))

  let governanceScript = C.SimpleScript $ C.RequireMOf 1 [C.RequireSignature (Wallet.verificationKeyHash Wallet.w1)]
  let govScriptHash = PLAV1.ScriptHash $ Builtins.toBuiltin $ C.serialiseToRawBytes $ C.hashScript governanceScript
  let redeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) govScriptHash

  let initGovernanceTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPublicKeyOutput @C.ConwayEra
            genesisUtxo
          BuildTx.createRefScriptInlineDatum (versioningScriptAddress genesisUtxo) governanceScript datum value
          BuildTx.mintPlutus (appliedVersioningPolicy genesisUtxo) redeemer (C.AssetName "Version oracle") (CA.Quantity 1)

  MockChainCoinSelection.balanceAndSubmit @C.ConwayEra mempty Wallet.w1 initGovernanceTx CoinSelection.TrailingChange []
