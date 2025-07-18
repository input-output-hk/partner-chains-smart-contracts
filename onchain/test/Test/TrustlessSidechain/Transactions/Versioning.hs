{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

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
import TrustlessSidechain.AlwaysPassingScripts qualified as AlwaysPassingScripts
import Prelude

-- | Type synonym for the test monad stack
--
-- @since 0.1
type TestM = C.ExceptT (CoinSelection.BalanceTxError C.ConwayEra) (MockChain.MockchainT C.ConwayEra IO)

-- | Bundles all values derived from a genesisUtxo and governance wallet for test setup
data VersioningTestSetup = VersioningTestSetup
  { vtsGenesisUtxo :: C.TxIn
  , vtsVersionOraclePolicyHash :: C.ScriptHash
  , vtsVersionOracleTokenValue :: C.Value
  , vtsVersionOracleDatum :: Versioning.VersionOracleDatum
  , vtsGovernanceScript :: C.SimpleScript
  , vtsGovernanceScriptHash :: PLAV1.ScriptHash
  , vtsInitializeOracleRedeemer :: Versioning.VersionOraclePolicyRedeemer
  , vtsVersioningValidatorHash :: C.ScriptHash
  , vtsVersioningScriptAddress :: C.AddressInEra C.ConwayEra
  }

mkVersioningTestSetup :: C.TxIn -> Wallet.Wallet -> VersioningTestSetup
mkVersioningTestSetup genesisUtxo governanceWallet =
  let versionOraclePolicyHash = mkVersioningPolicyHash genesisUtxo
      versionOracleTokenValue = versionOracleValue versionOraclePolicyHash
      versionOracleDatum = mkVersionOracleDatum versionOraclePolicyHash 32
      governanceScript = mkGovernanceScript governanceWallet
      governanceScriptHash = mkGovernanceScriptHash governanceWallet
      initializeOracleRedeemer = Versioning.InitializeVersionOracle (Versioning.VersionOracle 32) governanceScriptHash
      versioningValidatorHash = mkVersioningValidatorHash genesisUtxo
      versioningScriptAddress = mkVersioningScriptAddress genesisUtxo
  in VersioningTestSetup
      { vtsGenesisUtxo = genesisUtxo
      , vtsVersionOraclePolicyHash = versionOraclePolicyHash
      , vtsVersionOracleTokenValue = versionOracleTokenValue
      , vtsVersionOracleDatum = versionOracleDatum
      , vtsGovernanceScript = governanceScript
      , vtsGovernanceScriptHash = governanceScriptHash
      , vtsInitializeOracleRedeemer = initializeOracleRedeemer
      , vtsVersioningValidatorHash = versioningValidatorHash
      , vtsVersioningScriptAddress = versioningScriptAddress
      }

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
mkVersionOracleDatum :: C.ScriptHash -> Integer -> Versioning.VersionOracleDatum
mkVersionOracleDatum versionOraclePolicyHash scriptId =
  Versioning.VersionOracleDatum (Versioning.VersionOracle scriptId) (Value.currencySymbol $ C.serialiseToRawBytes versionOraclePolicyHash)

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

mkAlwaysSucceedsPolicy :: C.PlutusScript C.PlutusScriptV2
mkAlwaysSucceedsPolicy = CPlutusTx.compiledCodeToScript ($$(PlutusTx.compile [||AlwaysPassingScripts.mkAlwaysPassingPolicyUntyped||]) `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 (Builtins.mkConstr 0 []))

mkAlwaysSucceedsPolicyHash :: PLAV1.ScriptHash
mkAlwaysSucceedsPolicyHash = PLAV1.ScriptHash $ Builtins.toBuiltin $ C.serialiseToRawBytes $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 mkAlwaysSucceedsPolicy

-- | Helper to create the initial payment and return the resulting genesisUtxo
initGenesisUtxo :: TestM C.TxIn
initGenesisUtxo = do
  let initialPaymentTx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue $ fromIntegral versionOracleAdaAmount))
  initialPaymentResult <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 initialPaymentTx CoinSelection.TrailingChange []
  let initialTxId = C.getTxId $ C.getTxBody initialPaymentResult
  return $ C.TxIn initialTxId (C.TxIx 0)

-- | Transaction tests for Versioning
versioningTests :: TestTree
versioningTests =
  testGroup
    "Versioning contract tests"
    [ testCase "initGovernanceSucceeds" initGovernanceSucceeds
    , testCase "initGovernanceFailsWithoutSpendingGenesisUtxo" initGovernanceFailsWithoutSpendingGenesisUtxo
    , testCase "initGovernanceFailsWithoutAttachingDatum" initGovernanceFailsWithoutAttachingDatum
    , testCase "initGovernanceFailsWithoutAttachingReferenceScript" initGovernanceFailsWithoutAttachingReferenceScript
    , testCase "initGovernanceFailsWhenMintingIncorrectAmountOfVersionOracleToken" initGovernanceFailsWhenMintingIncorrectAmountOfVersionOracleToken
    , testCase "mintVersioningTokenFailsWithoutAttachingDatum" mintVersioningTokenFailsWithoutAttachingDatum
    , testCase "mintVersioningTokenFailsWithoutAttachingReferenceScript" mintVersioningTokenFailsWithoutAttachingReferenceScript
    , testCase "mintVersioningTokenFailsWhenNotSignedByGovernanceAuthority" mintVersioningTokenFailsWhenNotSignedByGovernanceAuthority
    , testCase "mintVersioningTokenFailsWhenMintingIncorrectAmountOfVersionOracleToken" mintVersioningTokenFailsWhenMintingIncorrectAmountOfVersionOracleToken
    , testCase "mintVersioningTokenFailsWhenGovernanceScriptUtxIsNotReferenced" mintVersioningTokenFailsWhenGovernanceScriptUtxIsNotReferenced
    , testCase "mintVersioningTokenSucceeds" mintVersioningTokenSucceeds
    , testCase "updateGovernanceSucceeds" updateGovernanceSucceeds
    ]

shouldFailWith :: String -> TestM () -> HUnit.Assertion
shouldFailWith errorMessage m = flip
  MockChainUtils.mockchainFails
  ( \case
      e | errorMessage `List.isInfixOf` show e -> return ()
      e -> error $ "Expected a " ++ errorMessage ++ ", got: " ++ show e
  ) $ Utils.failOnError m

initGovernanceFailsWithoutSpendingGenesisUtxo :: HUnit.Assertion
initGovernanceFailsWithoutSpendingGenesisUtxo = shouldFailWith "ERROR-VERSION-POLICY-01" $ do
  genesisUtxo <- initGenesisUtxo
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      governanceInitTx =
        BuildTx.execBuildTx $ do
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.SimpleScript vtsGovernanceScript) vtsVersionOracleDatum vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) vtsInitializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

initGovernanceFailsWithoutAttachingDatum :: HUnit.Assertion
initGovernanceFailsWithoutAttachingDatum = shouldFailWith "ERROR-VERSION-POLICY-02" $ do
  genesisUtxo <- initGenesisUtxo
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      governanceInitTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.SimpleScript vtsGovernanceScript) () vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) vtsInitializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

initGovernanceFailsWithoutAttachingReferenceScript :: HUnit.Assertion
initGovernanceFailsWithoutAttachingReferenceScript = shouldFailWith "ERROR-VERSION-POLICY-02" $ do
  genesisUtxo <- initGenesisUtxo
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      governanceInitTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
          BuildTx.payToScriptInlineDatum Defaults.networkId vtsVersioningValidatorHash vtsVersionOracleDatum C.NoStakeAddress vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) vtsInitializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

initGovernanceFailsWhenMintingIncorrectAmountOfVersionOracleToken :: HUnit.Assertion
initGovernanceFailsWhenMintingIncorrectAmountOfVersionOracleToken = shouldFailWith "ERROR-VERSION-POLICY-03" $ do
  genesisUtxo <- initGenesisUtxo
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      versionOracleTokenValue = fromList [(CA.AssetId (CA.PolicyId vtsVersionOraclePolicyHash) versionOracleAssetName, CA.Quantity 2), (CA.AdaAssetId, CA.Quantity $ fromIntegral versionOracleAdaAmount)]
      governanceInitTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.SimpleScript vtsGovernanceScript) vtsVersionOracleDatum versionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) vtsInitializeOracleRedeemer versionOracleAssetName (CA.Quantity 2)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

mintVersioningTokenFailsWithoutAttachingDatum :: HUnit.Assertion
mintVersioningTokenFailsWithoutAttachingDatum = shouldFailWith "ERROR-VERSION-POLICY-04" $ do
  (genesisUtxo, governanceTxIn, _)<- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      redeemer = Versioning.MintVersionOracle (Versioning.VersionOracle 35) mkAlwaysSucceedsPolicyHash
      mintVersioningTokenTx =
        BuildTx.execBuildTx $ do
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.PlutusScript C.PlutusScriptV2 mkAlwaysSucceedsPolicy) () vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) redeemer versionOracleAssetName (CA.Quantity 1)
          BuildTx.addReference governanceTxIn
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript vtsGovernanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript governanceTxIn)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 mintVersioningTokenTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

mintVersioningTokenFailsWithoutAttachingReferenceScript :: HUnit.Assertion
mintVersioningTokenFailsWithoutAttachingReferenceScript = shouldFailWith "ERROR-VERSION-POLICY-04" $ do
  (genesisUtxo, governanceTxIn, _) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      redeemer = Versioning.MintVersionOracle (Versioning.VersionOracle 35) mkAlwaysSucceedsPolicyHash
      datum = mkVersionOracleDatum vtsVersionOraclePolicyHash 35
      mintVersioningTokenTx =
        BuildTx.execBuildTx $ do
          BuildTx.payToScriptInlineDatum Defaults.networkId vtsVersioningValidatorHash datum C.NoStakeAddress vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) redeemer versionOracleAssetName (CA.Quantity 1)
          BuildTx.addReference governanceTxIn
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript vtsGovernanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript governanceTxIn)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 mintVersioningTokenTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

mintVersioningTokenFailsWhenNotSignedByGovernanceAuthority :: HUnit.Assertion
mintVersioningTokenFailsWhenNotSignedByGovernanceAuthority = shouldFailWith "ERROR-VERSION-POLICY-05" $ do
  (genesisUtxo, governanceTxIn, _) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      redeemer = Versioning.MintVersionOracle (Versioning.VersionOracle 35) mkAlwaysSucceedsPolicyHash
      datum = mkVersionOracleDatum vtsVersionOraclePolicyHash 35
      mintVersioningTokenTx =
        BuildTx.execBuildTx $ do
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.PlutusScript C.PlutusScriptV2 mkAlwaysSucceedsPolicy) datum vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) redeemer versionOracleAssetName (CA.Quantity 1)
          BuildTx.addReference governanceTxIn
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 mintVersioningTokenTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

mintVersioningTokenFailsWhenMintingIncorrectAmountOfVersionOracleToken :: HUnit.Assertion
mintVersioningTokenFailsWhenMintingIncorrectAmountOfVersionOracleToken = shouldFailWith "ERROR-VERSION-POLICY-06" $ do
  (genesisUtxo, governanceTxIn, _) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      redeemer = Versioning.MintVersionOracle (Versioning.VersionOracle 35) mkAlwaysSucceedsPolicyHash
      datum = mkVersionOracleDatum vtsVersionOraclePolicyHash 35
      value = fromList [(CA.AssetId (CA.PolicyId vtsVersionOraclePolicyHash) versionOracleAssetName, CA.Quantity 2), (CA.AdaAssetId, CA.Quantity $ fromIntegral versionOracleAdaAmount)]
      mintVersioningTokenTx =
        BuildTx.execBuildTx $ do
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.PlutusScript C.PlutusScriptV2 mkAlwaysSucceedsPolicy) datum value
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) redeemer versionOracleAssetName (CA.Quantity 2)
          BuildTx.addReference governanceTxIn
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript vtsGovernanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript governanceTxIn)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 mintVersioningTokenTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

mintVersioningTokenFailsWhenGovernanceScriptUtxIsNotReferenced :: HUnit.Assertion
mintVersioningTokenFailsWhenGovernanceScriptUtxIsNotReferenced = shouldFailWith "ERROR-VERSION-CURRENCY-01" $ do
  (genesisUtxo, governanceTxIn, _) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      redeemer = Versioning.MintVersionOracle (Versioning.VersionOracle 35) mkAlwaysSucceedsPolicyHash
      datum = mkVersionOracleDatum vtsVersionOraclePolicyHash 35
      mintVersioningTokenTx =
        BuildTx.execBuildTx $ do
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.PlutusScript C.PlutusScriptV2 mkAlwaysSucceedsPolicy) datum vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) redeemer versionOracleAssetName (CA.Quantity 1)
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript vtsGovernanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript governanceTxIn)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 mintVersioningTokenTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

mintVersioningTokenSucceeds :: HUnit.Assertion
mintVersioningTokenSucceeds = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  (genesisUtxo, governanceTxIn, _) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      redeemer = Versioning.MintVersionOracle (Versioning.VersionOracle 35) mkAlwaysSucceedsPolicyHash
      datum = mkVersionOracleDatum vtsVersionOraclePolicyHash 35
      mintVersioningTokenTx =
        BuildTx.execBuildTx $ do
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.PlutusScript C.PlutusScriptV2 mkAlwaysSucceedsPolicy) datum vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) redeemer versionOracleAssetName (CA.Quantity 1)
          BuildTx.addReference governanceTxIn
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript vtsGovernanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript governanceTxIn)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 mintVersioningTokenTx CoinSelection.TrailingChange [w2SigningKey]
  return ()

-- | Initialize governance and mint the version oracle token
initGovernance :: TestM (C.TxIn, C.TxIn, C.SimpleScript)
initGovernance = do
  genesisUtxo <- initGenesisUtxo
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      governanceInitTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPublicKeyOutput @C.ConwayEra genesisUtxo
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.SimpleScript vtsGovernanceScript) vtsVersionOracleDatum vtsVersionOracleTokenValue
          BuildTx.mintPlutus (mkVersioningPolicy genesisUtxo) vtsInitializeOracleRedeemer versionOracleAssetName (CA.Quantity 1)
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  _ <- MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 governanceInitTx CoinSelection.TrailingChange [w2SigningKey]
  Utxos.UtxoSet utxoSetMap <- Class.utxosByPaymentCredentials $ Set.fromList [C.PaymentCredentialByScript vtsVersioningValidatorHash]
  let utxoList = Map.toList utxoSetMap
      versionOracleAssetId = CA.AssetId (CA.PolicyId vtsVersionOraclePolicyHash) versionOracleAssetName
      versionOracleUtxos =
        filter
          ( \(_, (C.InAnyCardanoEra _ (CA.TxOut _ txOutValue _ _), _)) ->
              List.find (\(assetId, _) -> assetId == versionOracleAssetId) (Exts.toList $ CA.txOutValueToValue txOutValue) == Just (versionOracleAssetId, CA.Quantity 1)
          )
          utxoList
  (governanceTxIn, governanceScript) <- case versionOracleUtxos of
    (governanceTxIn, (C.InAnyCardanoEra _ (CA.TxOut _ _ _ (C.ReferenceScript _ (C.ScriptInAnyLang C.SimpleScriptLanguage (C.SimpleScript governanceScript)))), _)) : _ -> return (governanceTxIn, governanceScript)
    _ -> error $ show utxoList
  return (genesisUtxo, governanceTxIn, governanceScript)

initGovernanceSucceeds :: HUnit.Assertion
initGovernanceSucceeds = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  (genesisUtxo, _, governanceReferenceScript) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1

  case governanceReferenceScript == vtsGovernanceScript of
    True -> return ()
    False -> error "Governance reference script is not the expected script"

  return ()

-- | Test updating governance by spending the version oracle UTxO
updateGovernanceSucceeds :: HUnit.Assertion
updateGovernanceSucceeds = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  (genesisUtxo, versionOracleTxIn, _) <- initGovernance
  let VersioningTestSetup{..} = mkVersioningTestSetup genesisUtxo Wallet.w1
      w2SigningKey = Operator.toShelleyWitnessSigningKey $ Operator.PESigning $ Wallet.getWallet Wallet.w2
  let newGovernanceScript = mkGovernanceScript Wallet.w2
      updateGovernanceTx =
        BuildTx.execBuildTx $ do
          BuildTx.spendPlutusInlineDatum @_ @_ @C.ConwayEra
            versionOracleTxIn
            (mkVersioningValidator genesisUtxo)
            (Versioning.VersionOracle 32)
          BuildTx.createRefScriptInlineDatum vtsVersioningScriptAddress (C.SimpleScript newGovernanceScript) vtsVersionOracleDatum vtsVersionOracleTokenValue
          BuildTx.addMintWithTxBody (CA.PolicyId $ C.hashScript (C.SimpleScript vtsGovernanceScript)) (CA.AssetName "Governance Token") (CA.Quantity 1) (const $ InternalScript.SimpleScriptWitness InternalScript.SimpleScriptInConway $ InternalScript.SReferenceScript versionOracleTxIn)
  MockChainCoinSelection.tryBalanceAndSubmit @C.ConwayEra mempty Wallet.w1 updateGovernanceTx CoinSelection.TrailingChange [w2SigningKey]
