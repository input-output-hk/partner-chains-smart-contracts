module ICSAuthorityToken where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.ICSAuthorityToken
import TrustlessSidechain.Types (
  ICSAuthorityTokenRedeemer (
    ICSAuthorityTokenBurn,
    ICSAuthorityTokenMint
  ),
 )
import TrustlessSidechain.Versioning (VersionOracleConfig)
import Prelude

policyTests :: TestTree
policyTests =
  testGroup
    "ICS Authority Token policy"
    [ icsAuthorityTokenMintPassing
    , icsAuthorityTokenBurnPassing
    , icsAuthorityTokenMintFailing
    , icsAuthorityTokenBurnFailing
    , icsAuthorityTokenWrongContextFailing
    ]

-- Test successful minting with governance authority signature
icsAuthorityTokenMintPassing :: TestTree
icsAuthorityTokenMintPassing =
  expectSuccess "should pass minting with governance authority signature" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      ICSAuthorityTokenMint
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          -- signed by governance authority
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken 10
      )

-- Test successful burning with governance authority signature  
icsAuthorityTokenBurnPassing :: TestTree
icsAuthorityTokenBurnPassing =
  expectSuccess "should pass burning with governance authority signature" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      ICSAuthorityTokenBurn
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          -- signed by governance authority
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken (-5)
      )

-- Test failed minting without governance authority signature
icsAuthorityTokenMintFailing :: TestTree
icsAuthorityTokenMintFailing =
  expectFail "should fail minting without governance authority signature (ERROR-ICS-AUTHORITY-TOKEN-POLICY-01)" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      ICSAuthorityTokenMint
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken 10
      )

-- Test failed burning without governance authority signature
icsAuthorityTokenBurnFailing :: TestTree
icsAuthorityTokenBurnFailing =
  expectFail "should fail burning without governance authority signature (ERROR-ICS-AUTHORITY-TOKEN-POLICY-02)" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      ICSAuthorityTokenBurn
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting icsAuthorityTokenCurrSym
          & _scriptContextTxInfo . _txInfoMint <>~ icsAuthorityToken (-5)
      )

-- Test wrong script context (should never happen)
icsAuthorityTokenWrongContextFailing :: TestTree
icsAuthorityTokenWrongContextFailing =
  expectFail "should fail with wrong script context (ERROR-ICS-AUTHORITY-TOKEN-POLICY-03)" $
    runMintingPolicy
      Test.genesisUtxo
      Test.versionOracleConfig
      ICSAuthorityTokenMint
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending (V2.TxOutRef "abcd0123" 0)
          & _scriptContextTxInfo . _txInfoInputs <>~ [emptyTxInInfo & _txInInfoResolved .~ Test.governanceTokenUtxo]
          & _scriptContextTxInfo . _txInfoMint <>~ Test.governanceToken
      )

-- Helper functions

icsAuthorityTokenCurrSym :: V2.CurrencySymbol
icsAuthorityTokenCurrSym = V2.CurrencySymbol . V2.getScriptHash $ icsAuthorityTokenScriptHash

icsAuthorityTokenScriptHash :: V2.ScriptHash  
icsAuthorityTokenScriptHash = V2.ScriptHash "icsAuthorityTokenScriptHash"

icsAuthorityToken :: Integer -> V2.Value
icsAuthorityToken = V2.singleton icsAuthorityTokenCurrSym icsAuthorityTokenName
  where
    icsAuthorityTokenName :: V2.TokenName
    icsAuthorityTokenName = V2.TokenName "ICS Authority Token"

runMintingPolicy :: V2.TxOutRef -> VersionOracleConfig -> ICSAuthorityTokenRedeemer -> V2.ScriptContext -> BuiltinUnit
runMintingPolicy genesisUtxo vc redeemer ctx =
  mkMintingPolicyUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData vc)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)