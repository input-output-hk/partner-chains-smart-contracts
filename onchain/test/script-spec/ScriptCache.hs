module ScriptCache where

import Control.Lens
import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TrustlessSidechain.ScriptCache
import Prelude

validatorTests :: TestTree
validatorTests =
  testGroup
    "committee-candidate validator"
    [ scriptCacheValidatorPassing
    , scriptCacheValidatorFailing
    ]

scriptCacheValidatorPassing :: TestTree
scriptCacheValidatorPassing =
  expectSuccess "should pass" $
    runValidator
      pubKeyHash
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          & _scriptContextTxInfo . _txInfoSignatories <>~ [pubKeyHash]
      )

scriptCacheValidatorFailing :: TestTree
scriptCacheValidatorFailing =
  expectFail "should fail if not signed by the original submitter (ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01)" $
    runValidator
      pubKeyHash
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          & _scriptContextTxInfo . _txInfoSignatories <>~ [wrongPubKeyHash]
      )

pubKeyHash :: V2.PubKeyHash
pubKeyHash = V2.PubKeyHash "01230123012301230123012301230123012301230123012301230123"

wrongPubKeyHash :: V2.PubKeyHash
wrongPubKeyHash = V2.PubKeyHash "01230123012301230123012301230123012301230123012301230124"

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

runValidator :: V2.PubKeyHash -> V2.ScriptContext -> BuiltinUnit
runValidator pkh ctx =
  mkScriptCacheUntyped
    (toBuiltinData pkh)
    (toBuiltinData ())
    (toBuiltinData ())
    (toBuiltinData ctx)
