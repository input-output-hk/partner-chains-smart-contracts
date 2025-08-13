module CommitteeCandidateValidator where

import Control.Lens
import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptSpecUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.CommitteeCandidateValidator
import TrustlessSidechain.Types
import Prelude

validatorTests :: TestTree
validatorTests =
  testGroup
    "committee-candidate validator"
    [ committeeCandidateValidatorPassing
    , committeeCandidateValidatorFailing
    ]

committeeCandidateValidatorPassing :: TestTree
committeeCandidateValidatorPassing =
  expectSuccess "should pass" $
    runValidator
      Test.genesisUtxo
      committeeCandidateValidatorDatum
      Test.dummyBuiltinData
      ( emptyScriptContext
          & _scriptContextPurpose .~ V2.Spending someTxOutRef
          & _scriptContextTxInfo . _txInfoSignatories <>~ [pubKeyHash]
      )

committeeCandidateValidatorFailing :: TestTree
committeeCandidateValidatorFailing =
  expectFail "should fail if not signed by the original submitter (ERROR-COMMITTEE-CANDIDATE-VALIDATOR-01)" $
    runValidator
      Test.genesisUtxo
      committeeCandidateValidatorDatum
      Test.dummyBuiltinData
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

committeeCandidateValidatorDatum :: VersionedGenericDatum V2.PubKeyHash
committeeCandidateValidatorDatum = VersionedGenericDatum pubKeyHash Test.dummyBuiltinData 0

runValidator :: V2.TxOutRef -> VersionedGenericDatum V2.PubKeyHash -> BuiltinData -> V2.ScriptContext -> BuiltinUnit
runValidator genesisUtxo datum redeemer ctx =
  committeeCandidateValidatorUntyped
    (toBuiltinData genesisUtxo)
    (toBuiltinData datum)
    (toBuiltinData redeemer)
    (toBuiltinData ctx)
