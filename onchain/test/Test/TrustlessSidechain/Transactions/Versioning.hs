{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Test.TrustlessSidechain.Transactions.Versioning (versioningTests, initGovernance, mkVersioningPolicyHash, mkVersioningValidatorHash, mkVersioningScriptAddress) where

import Cardano.Api qualified as CA
import Cardano.Api.Internal.Script as InternalScript
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Hashes qualified as Hash
import Convex.BuildTx qualified as BuildTx
import Convex.Class qualified as Class
import Convex.CoinSelection qualified as CoinSelection
import Convex.MockChain qualified as MockChain
import Convex.MockChain.CoinSelection qualified as MockChainCoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils qualified as MockChainUtils
import Convex.PlutusTx qualified as CPlutusTx
import Convex.Utils qualified as Utils
import Convex.Utxos qualified as Utxos
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Convex.Wallet.Operator qualified as Operator
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (fromString)
import GHC.Exts (IsList (..), fromList)
import GHC.Exts qualified as Exts
import GHC.Num qualified as Num
import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 qualified as PLAV1
import PlutusLedgerApi.V1.Data.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import Test.HUnit qualified as HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import TrustlessSidechain.Versioning qualified as Versioning
import Prelude

-- | Type synonym for the test monad stack
--
-- @since 0.1
type TestM = C.ExceptT (CoinSelection.BalanceTxError C.ConwayEra) (MockChain.MockchainT C.ConwayEra IO)

-- | Constants
versionOracleAssetName :: CA.AssetName
versionOracleAssetName = CA.AssetName "Version oracle"

versionOracleAdaAmount :: Integer
versionOracleAdaAmount = 10_000_000

-- | Construct the value for the version oracle UTxO
versionOracleValue :: C.ScriptHash -> C.Value
versionOracleValue versionOraclePolicyHash =
  fromList
    [ (CA.AssetId (CA.PolicyId versionOraclePolicyHash) versionOracleAssetName, CA.Quantity 1)
    , (CA.AdaAssetId, CA.Quantity $ fromIntegral versionOracleAdaAmount)
    ]

-- | Construct the datum for the version oracle
mkVersionOracleDatum :: C.ScriptHash -> Versioning.VersionOracleDatum
mkVersionOracleDatum versionOraclePolicyHash =
  Versioning.VersionOracleDatum (Versioning.VersionOracle 32) (Value.currencySymbol $ C.serialiseToRawBytes versionOraclePolicyHash)

-- | Governance script helpers
mkGovernanceScript :: Wallet.Wallet -> C.SimpleScript
mkGovernanceScript wallet = C.RequireMOf 1 [C.RequireSignature (Wallet.verificationKeyHash wallet)]

mkGovernanceScriptHash :: Wallet.Wallet -> PLAV1.ScriptHash
mkGovernanceScriptHash wallet =
  PLAV1.ScriptHash $ Builtins.toBuiltin $ C.serialiseToRawBytes $ C.hashScript (C.SimpleScript $ mkGovernanceScript wallet)

-- | Versioning helpers
mkVersioningValidator :: C.TxIn -> C.PlutusScript C.PlutusScriptV2
mkVersioningValidator (C.TxIn (CA.TxId txIdHash) (CA.TxIx txIndex)) = CPlutusTx.compiledCodeToScript compiledValidator
  where
    compiledCodeResult = ($$(PlutusTx.compile [||Versioning.mkVersionOracleValidatorUntyped||]) `PlutusTx.applyCode` PlutusTx.liftCode plcVersion100 txOutRefBuiltin)
    cborHash = Hash.originalBytes txIdHash
    txOutRefBuiltin = Builtins.mkConstr 0 [Builtins.mkB $ Builtins.toBuiltin cborHash, Builtins.mkI $ Num.integerFromWord txIndex]
    compiledValidator = case compiledCodeResult of
      Left err -> error $ "Failed to compile versioning validator: " ++ show err
      Right compiled -> compiled

mkVersioningValidatorHash :: C.TxIn -> C.ScriptHash
mkVersioningValidatorHash = C.hashScript . C.PlutusScript C.PlutusScriptV2 . mkVersioningValidator

mkVersioningPolicy :: C.TxIn -> C.PlutusScript C.PlutusScriptV2
mkVersioningPolicy txIn@(C.TxIn (CA.TxId txIdHash) (CA.TxIx txIndex)) = CPlutusTx.compiledCodeToScript compiledPolicy
  where
    validatorAddress = PlutusTx.liftCode plcVersion100 $ Builtins.mkConstr 0 [Builtins.mkConstr 1 [Builtins.mkB $ Builtins.toBuiltin $ C.serialiseToRawBytes $ mkVersioningValidatorHash txIn], Builtins.mkConstr 1 []]
    cborHash = Hash.originalBytes txIdHash
    txOutRefBuiltin = PlutusTx.liftCode plcVersion100 $ Builtins.mkConstr 0 [Builtins.mkConstr 0 [Builtins.mkB $ Builtins.toBuiltin cborHash], Builtins.mkI $ Num.integerFromWord txIndex]
    compiledCode = ($$(PlutusTx.compile [||Versioning.mkVersionOraclePolicyUntyped||]) `PlutusTx.unsafeApplyCode` txOutRefBuiltin)
    compiledCodeResult = (compiledCode `PlutusTx.applyCode` validatorAddress)
    compiledPolicy = case compiledCodeResult of
      Left err -> error $ "Failed to compile versioning policy: " ++ show err
      Right compiled -> compiled

mkVersioningPolicyHash :: C.TxIn -> C.ScriptHash
mkVersioningPolicyHash = C.hashScript . C.PlutusScript C.PlutusScriptV2 . mkVersioningPolicy

mkVersioningScriptAddress :: C.TxIn -> C.AddressInEra C.ConwayEra
mkVersioningScriptAddress genesisUtxo = Utils.scriptAddress Defaults.networkId $ mkVersioningValidator genesisUtxo

-- | Transaction tests for Versioning
versioningTests :: TestTree
versioningTests =
  testGroup
    "Versioning contract tests"
    [ testCase "initGovernanceSucceeds" initGovernanceSucceeds
    , testCase "initGovernanceFailsWithoutSpendingGenesisUtxo" initGovernanceFailsWithoutSpendingGenesisUtxo
    , testCase "initGovernanceFailsWithoutAttachingDatum" initGovernanceFailsWithoutAttachingDatum
    , testCase "initGovernanceFailsWithoutAttachingReferenceScript" initGovernanceFailsWithoutAttachingReferenceScript
    , testCase "initGovernanceFailsWithoutMintingVersionOracleToken" initGovernanceFailsWithoutMintingVersionOracleToken
    , testCase "updateGovernanceSucceeds" updateGovernanceSucceeds
    ]

initGovernanceFailsWithoutSpendingGenesisUtxo :: HUnit.Assertion
initGovernanceFailsWithoutSpendingGenesisUtxo = flip
  MockChainUtils.mockchainFails
  ( \case
      e | "ERROR-VERSION-POLICY-01" `List.isInfixOf` show e -> return ()
      e -> error $ "Expected a ERROR-VERSION-POLICY-01, got: " ++ show e
  )
  $ Utils.failOnError
  $ do
    let initialPaymentTx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue $ fromIntegral versionOracleAdaAmount))
    initialPaymentResult <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 initialPaymentTx CoinSelection.TrailingChange []
    let initialTxId = C.getTxId $ C.getTxBody initialPaymentResult
        genesisUtxo = C.TxIn initialTxId (C.TxIx 0)
        versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
        versionOracleTokenValue = versionOracleValue versionOraclePolicyHash
        versionOracleDatum = mkVersionOracleDatum versionOraclePolicyHash
        governanceScript = mkGovernanceScript Wallet.w1
        governanceScriptHash = mkGovernanceScriptHash Wallet.w1
        initializeOracleRedeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) governanceScriptHash
        governanceInitTx =
          BuildTx.execBuildTx $ do
            BuildTx.createRefScriptInlineDatum (mkVersioningScriptAddress genesisUtxo) (C.SimpleScript governanceScript) versionOracleDatum versionOracleTokenValue
            BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) initializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
        w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
    _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
    return ()

initGovernanceFailsWithoutAttachingDatum :: HUnit.Assertion
initGovernanceFailsWithoutAttachingDatum = flip
  MockChainUtils.mockchainFails
  ( \case
      e | "ERROR-VERSION-POLICY-02" `List.isInfixOf` show e -> return ()
      e -> error $ "Expected a ERROR-VERSION-POLICY-02, got: " ++ show e
  )
  $ Utils.failOnError
  $ do
    let initialPaymentTx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue $ fromIntegral versionOracleAdaAmount))
    initialPaymentResult <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 initialPaymentTx CoinSelection.TrailingChange []
    let initialTxId = C.getTxId $ C.getTxBody initialPaymentResult
        genesisUtxo = C.TxIn initialTxId (C.TxIx 0)
        versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
        versionOracleTokenValue = versionOracleValue versionOraclePolicyHash
        governanceScript = mkGovernanceScript Wallet.w1
        governanceScriptHash = mkGovernanceScriptHash Wallet.w1
        initializeOracleRedeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) governanceScriptHash
        governanceInitTx =
          BuildTx.execBuildTx $ do
            BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
            BuildTx.createRefScriptInlineDatum (mkVersioningScriptAddress genesisUtxo) (C.SimpleScript governanceScript) () versionOracleTokenValue
            BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) initializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
        w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
    _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
    return ()

initGovernanceFailsWithoutAttachingReferenceScript :: HUnit.Assertion
initGovernanceFailsWithoutAttachingReferenceScript = flip
  MockChainUtils.mockchainFails
  ( \case
      e | "ERROR-VERSION-POLICY-02" `List.isInfixOf` show e -> return ()
      e -> error $ "Expected a ERROR-VERSION-POLICY-02, got: " ++ show e
  )
  $ Utils.failOnError
  $ do
    let initialPaymentTx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue $ fromIntegral versionOracleAdaAmount))
    initialPaymentResult <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 initialPaymentTx CoinSelection.TrailingChange []
    let initialTxId = C.getTxId $ C.getTxBody initialPaymentResult
        genesisUtxo = C.TxIn initialTxId (C.TxIx 0)
        versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
        versionOracleTokenValue = versionOracleValue versionOraclePolicyHash
        versionOracleDatum = mkVersionOracleDatum versionOraclePolicyHash
        governanceScriptHash = mkGovernanceScriptHash Wallet.w1
        initializeOracleRedeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) governanceScriptHash
        governanceInitTx =
          BuildTx.execBuildTx $ do
            BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
            BuildTx.payToScriptInlineDatum Defaults.networkId (mkVersioningValidatorHash genesisUtxo) versionOracleDatum C.NoStakeAddress versionOracleTokenValue
            BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) initializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
        w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
    _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
    return ()

initGovernanceFailsWithoutMintingVersionOracleToken :: HUnit.Assertion
initGovernanceFailsWithoutMintingVersionOracleToken = flip
  MockChainUtils.mockchainFails
  ( \case
      e | "ERROR-VERSION-POLICY-03" `List.isInfixOf` show e -> return ()
      e -> error $ "Expected a ERROR-VERSION-POLICY-03, got: " ++ show e
  )
  $ Utils.failOnError
  $ do
    let initialPaymentTx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue $ fromIntegral versionOracleAdaAmount))
    initialPaymentResult <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 initialPaymentTx CoinSelection.TrailingChange []
    let initialTxId = C.getTxId $ C.getTxBody initialPaymentResult
        genesisUtxo = C.TxIn initialTxId (C.TxIx 0)
        versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
        versionOracleTokenValue = fromList [(CA.AssetId (CA.PolicyId versionOraclePolicyHash) versionOracleAssetName, CA.Quantity 2), (CA.AdaAssetId, CA.Quantity $ fromIntegral versionOracleAdaAmount)]
        versionOracleDatum = mkVersionOracleDatum versionOraclePolicyHash
        governanceScript = mkGovernanceScript Wallet.w1
        governanceScriptHash = mkGovernanceScriptHash Wallet.w1
        initializeOracleRedeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) governanceScriptHash
        governanceInitTx =
          BuildTx.execBuildTx $ do
            BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
            BuildTx.createRefScriptInlineDatum (mkVersioningScriptAddress genesisUtxo) (C.SimpleScript governanceScript) versionOracleDatum versionOracleTokenValue
            BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) initializeOracleRedeemer versionOracleAssetName (CA.Quantity 2)
        w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
    _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
    return ()

-- | Initialize governance and mint the version oracle token
initGovernance :: TestM C.TxIn
initGovernance = do
  let initialPaymentTx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue $ fromIntegral versionOracleAdaAmount))
  initialPaymentResult <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 initialPaymentTx CoinSelection.TrailingChange []
  let initialTxId = C.getTxId $ C.getTxBody initialPaymentResult
      genesisUtxo = C.TxIn initialTxId (C.TxIx 0)
      versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
      versionOracleTokenValue = versionOracleValue versionOraclePolicyHash
      versionOracleDatum = mkVersionOracleDatum versionOraclePolicyHash
      governanceScript = mkGovernanceScript Wallet.w1
      governanceScriptHash = mkGovernanceScriptHash Wallet.w1
      initializeOracleRedeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) governanceScriptHash
      governanceInitTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
          BuildTx.createRefScriptInlineDatum (mkVersioningScriptAddress genesisUtxo) (C.SimpleScript governanceScript) versionOracleDatum versionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) initializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
  return genesisUtxo

initGovernanceSucceeds :: HUnit.Assertion
initGovernanceSucceeds = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  genesisUtxo <- initGovernance
  Utxos.UtxoSet utxoSetMap <- Class.utxosByPaymentCredentials $ Set.fromList [C.PaymentCredentialByScript $ mkVersioningValidatorHash genesisUtxo]
  let utxoList = Map.toList utxoSetMap
      versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
      versionOracleAssetId = CA.AssetId (CA.PolicyId versionOraclePolicyHash) versionOracleAssetName
      versionOracleUtxos =
        filter
          ( \(_, (C.InAnyCardanoEra _ (CA.TxOut _ txOutValue _ _), _)) ->
              List.find (\(assetId, _) -> assetId == versionOracleAssetId) (Exts.toList $ CA.txOutValueToValue txOutValue) == Just (versionOracleAssetId, CA.Quantity 1)
          )
          utxoList
  governanceReferenceScript <- case versionOracleUtxos of
    (_, (C.InAnyCardanoEra _ (CA.TxOut _ _ _ (C.ReferenceScript _ (C.ScriptInAnyLang C.SimpleScriptLanguage (C.SimpleScript governanceScript)))), _)) : _ -> return governanceScript
    _ -> error $ show utxoList

  case governanceReferenceScript == (mkGovernanceScript Wallet.w1) of
    True -> return ()
    False -> error "Governance reference script is not the expected script"

  return ()

-- | Test updating governance by spending the version oracle UTxO
updateGovernanceSucceeds :: HUnit.Assertion
updateGovernanceSucceeds = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  genesisUtxo <- initGovernance
  let versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
      versionOracleTokenValue = versionOracleValue versionOraclePolicyHash
      versionOracleDatum = mkVersionOracleDatum versionOraclePolicyHash
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
      governanceScript = mkGovernanceScript Wallet.w1
  Utxos.UtxoSet utxoSetMap <- Class.utxosByPaymentCredentials $ Set.fromList [C.PaymentCredentialByScript $ mkVersioningValidatorHash genesisUtxo]
  let utxoList = Map.toList utxoSetMap
      versionOracleAssetId = CA.AssetId (CA.PolicyId versionOraclePolicyHash) versionOracleAssetName
      versionOracleUtxos =
        filter
          ( \(_, (C.InAnyCardanoEra _ (CA.TxOut _ txOutValue _ _), _)) ->
              List.find (\(assetId, _) -> assetId == versionOracleAssetId) (Exts.toList $ CA.txOutValueToValue txOutValue) == Just (versionOracleAssetId, CA.Quantity 1)
          )
          utxoList
  versionOracleTxIn <- case versionOracleUtxos of
    [] -> error $ show utxoList
    (txIn, _) : _ -> return txIn
  let newGovernanceScript = mkGovernanceScript Wallet.w2
      updateGovernanceTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPlutusInlineDatum @_ @_ @C.ConwayEra
            versionOracleTxIn
            (mkVersioningValidator genesisUtxo)
            (Versioning.VersionOracle 32)
          BuildTx.createRefScriptInlineDatum (mkVersioningScriptAddress genesisUtxo) (C.SimpleScript newGovernanceScript) versionOracleDatum versionOracleTokenValue
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript governanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript versionOracleTxIn)
  MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 updateGovernanceTx CoinSelection.TrailingChange [w2SigningKey]
