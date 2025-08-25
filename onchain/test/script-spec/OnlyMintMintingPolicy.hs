module OnlyMintMintingPolicy where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.OnlyMintMintingPolicy

policyTests :: TestTree
policyTests =
  testGroup
    "only mint minting policy"
    [ onlyMintMintingPolicyPassing
    , onlyMintMintingPolicyFailing1
    , onlyMintMintingPolicyFailing2
    ]

onlyMintMintingPolicyPassing :: TestTree
onlyMintMintingPolicyPassing =
  expectSuccess "should pass" $
    runMintingPolicy
      Test.genesisUtxo
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting onlyMintCurrSym
          & _scriptContextTxInfo . _txInfoMint <>~ onlyMintToken 2
      )

onlyMintMintingPolicyFailing1 :: TestTree
onlyMintMintingPolicyFailing1 =
  expectFail "should fail if tokens are burned (ERROR-DUMMY-MINTING-01)" $
    runMintingPolicy
      Test.genesisUtxo
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Minting onlyMintCurrSym
          & _scriptContextTxInfo . _txInfoMint <>~ onlyMintToken (-1)
      )

onlyMintMintingPolicyFailing2 :: TestTree
onlyMintMintingPolicyFailing2 =
  expectFail "should fail if there is Spend script purpose (ERROR-DUMMY-MINTING-02)" $
    runMintingPolicy
      Test.genesisUtxo
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          & _scriptContextTxInfo . _txInfoMint <>~ onlyMintToken 2
      )

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

onlyMintCurrSym :: V2.CurrencySymbol
onlyMintCurrSym = V2.CurrencySymbol . V2.getScriptHash $ onlyMintValidatorScriptHash

onlyMintValidatorScriptHash :: V2.ScriptHash
onlyMintValidatorScriptHash = V2.ScriptHash "onlyMintValidatorScriptHash"

onlyMintToken :: Integer -> V2.Value
onlyMintToken = V2.singleton onlyMintCurrSym onlyMintTokenName
  where
    onlyMintTokenName :: V2.TokenName
    onlyMintTokenName = V2.TokenName "only mint"

runMintingPolicy :: V2.TxOutRef -> V2.ScriptContext -> BuiltinUnit
runMintingPolicy genesisUtxo ctx =
  mkOnlyMintMintingPolicyUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData ())
    (toBuiltinData ctx)
