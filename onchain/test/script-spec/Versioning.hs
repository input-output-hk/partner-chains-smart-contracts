module Versioning where

import Control.Lens
import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.Types
import TrustlessSidechain.Versioning
import Prelude

-- tests

versioningPolicyInitializeTestPassing :: TestTree
versioningPolicyInitializeTestPassing =
  expectSuccess "init versioning policy should pass" $
    runVersioningPolicyWithParams
      passingVersioningPolicyInitializeParams

versioningPolicyInitializeTestFailing01 :: TestTree
versioningPolicyInitializeTestFailing01 =
  expectFail "init versioning policy should fail on empty inputs" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyInitializeParams
          & (_ctx . _scriptContextTxInfo . _txInfoOutputs)
            .~ []
      )

versioningPolicyInitializeTestFailing02 :: TestTree
versioningPolicyInitializeTestFailing02 =
  expectFail "init versioning policy should fail on empty outputs" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyInitializeParams
          & (_ctx . _scriptContextTxInfo . _txInfoInputs)
            .~ []
      )

versioningPolicyInitializeTestFailing03 :: TestTree
versioningPolicyInitializeTestFailing03 =
  expectFail "init versioning policy should fail on invalid token being minted" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyInitializeParams
          & (_ctx . _scriptContextTxInfo . _txInfoMint)
            .~ V2.singleton (V2.CurrencySymbol "WRONG CurrSym") (V2.TokenName "WRONG token name") 1
      )

versioningPolicyMintTestPassing :: TestTree
versioningPolicyMintTestPassing =
  expectSuccess "mint versioning policy should pass" $
    runVersioningPolicyWithParams passingVersioningPolicyMintParams

versioningPolicyMintTestFailing04 :: TestTree
versioningPolicyMintTestFailing04 =
  expectFail "mint versioning policy should fail on empty outputs" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyMintParams
          & (_ctx . _scriptContextTxInfo . _txInfoInputs)
            .~ []
      )

versioningPolicyMintTestFailing05 :: TestTree
versioningPolicyMintTestFailing05 =
  expectFail "mint versioning policy should fail if not signed by gov auth" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyMintParams
          & (_ctx . _scriptContextTxInfo . _txInfoInputs) %~ take 1
      )

versioningPolicyMintTestFailing06 :: TestTree
versioningPolicyMintTestFailing06 =
  expectFail "mint versioning policy should fail on invalid token being minted" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyMintParams
          & (_ctx . _scriptContextTxInfo . _txInfoMint)
            .~ V2.singleton (V2.CurrencySymbol "WRONG CurrSym") (V2.TokenName "WRONG token name") 1
      )

versioningPolicyBurnTestPassing :: TestTree
versioningPolicyBurnTestPassing =
  expectSuccess "Burn versioning policy should pass" $
    runVersioningPolicyWithParams passingVersioningPolicyBurnParams

versioningPolicyBurnTestFailing07 :: TestTree
versioningPolicyBurnTestFailing07 =
  expectFail "Burn versioning policy should fail on empty outputs" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyBurnParams
          & (_ctx . _scriptContextTxInfo . _txInfoInputs)
            .~ []
      )

versioningPolicyBurnTestFailing08 :: TestTree
versioningPolicyBurnTestFailing08 =
  expectFail "Burn versioning policy should fail if not signed by gov auth" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyBurnParams
          & (_ctx . _scriptContextTxInfo . _txInfoInputs) %~ take 1
      )

versioningPolicyBurnTestFailing09 :: TestTree
versioningPolicyBurnTestFailing09 =
  expectFail "Burn versioning policy should fail on invalid token being Burned" $
    runVersioningPolicyWithParams
      ( passingVersioningPolicyBurnParams
          & (_ctx . _scriptContextTxInfo . _txInfoMint)
            .~ V2.singleton (V2.CurrencySymbol "WRONG CurrSym") (V2.TokenName "WRONG token name") 1
      )

versioningTokenUtxo :: V2.TxOut
versioningTokenUtxo =
  mkTxOut
    Test.versionValidatorAddress
    Test.versionOracleToken
    Test.versionOracleDatum
    Test.versioningValidatorScriptHash

governanceTokenUtxo :: V2.TxOut
governanceTokenUtxo =
  mkTxOut
    Test.versionValidatorAddress
    Test.versionOracleToken
    Test.governanceVersionOracleDatum
    Test.governanceValidatorScriptHash

-- test runner

data VersioningPolicyParams = VersioningPolicyParams
  { genesisUtxo :: V2.TxOutRef
  , validatorAddress :: V2.Address
  , redeemer :: VersionOraclePolicyRedeemer
  , ctx :: V2.ScriptContext
  }

_ctx :: Lens' VersioningPolicyParams V2.ScriptContext
_ctx f a@VersioningPolicyParams {..} =
  fmap
    (\ctx' -> a {ctx = ctx'})
    (f ctx)

runVersioningPolicyWithParams :: VersioningPolicyParams -> BuiltinUnit
runVersioningPolicyWithParams VersioningPolicyParams {..} =
  mkVersionOraclePolicyUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData validatorAddress)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)

passingVersioningPolicyInitializeParams :: VersioningPolicyParams
passingVersioningPolicyInitializeParams =
  VersioningPolicyParams
    { genesisUtxo = Test.genesisUtxo
    , validatorAddress = Test.versionValidatorAddress
    , redeemer = InitializeVersionOracle Test.versionOracle Test.versioningValidatorScriptHash
    , ctx =
        emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.versionOracleToken
    }

passingVersioningPolicyMintParams :: VersioningPolicyParams
passingVersioningPolicyMintParams =
  VersioningPolicyParams
    { genesisUtxo = Test.genesisUtxo
    , validatorAddress = Test.versionValidatorAddress
    , redeemer = MintVersionOracle Test.versionOracle Test.versioningValidatorScriptHash
    , ctx =
        emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoOutRef .~ Test.genesisUtxo]
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoOutputs <>~ [versioningTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ (Test.versionOracleToken <> Test.governanceToken)
    }

passingVersioningPolicyBurnParams :: VersioningPolicyParams
passingVersioningPolicyBurnParams =
  VersioningPolicyParams
    { genesisUtxo = Test.genesisUtxo
    , validatorAddress = Test.versionValidatorAddress
    , redeemer = BurnVersionOracle Test.versionOracle
    , ctx =
        emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting Test.versioningCurrSym
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
    }
