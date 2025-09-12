module CommitteeCandidateValidator (execCosts) where

import Control.Lens
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import PlutusTx.Builtins.Internal (BuiltinUnit (..))
import ScriptPerfUtils
import Test.Tasty
import TestValues qualified as Test
import TrustlessSidechain.CommitteeCandidateValidator
import TrustlessSidechain.Types qualified as Types

execCosts :: TestTree
execCosts =
  testGroup
    "CommitteeCandidateValidator scripts ExBudget"
    [ goldenPerf "committeeCandidateValidator" committeeCandidateValidator
    ]

committeeCandidateValidator :: CompiledCode BuiltinUnit
committeeCandidateValidator =
  runValidator
    Test.genesisUtxo
    committeeCandidateValidatorDatum
    Test.dummyBuiltinData
    ( emptyScriptContext
        & _scriptContextPurpose .~ V2.Spending someTxOutRef
        & _scriptContextTxInfo . _txInfoSignatories <>~ [pubKeyHash]
    )

pubKeyHash :: V2.PubKeyHash
pubKeyHash = V2.PubKeyHash "01230123012301230123012301230123012301230123012301230123"

someTxOutRef :: V2.TxOutRef
someTxOutRef = V2.TxOutRef "abcd0123" 0

committeeCandidateValidatorDatum :: Types.VersionedGenericDatum V2.PubKeyHash
committeeCandidateValidatorDatum = Types.VersionedGenericDatum pubKeyHash Test.dummyBuiltinData 0

runValidator :: V2.TxOutRef -> Types.VersionedGenericDatum V2.PubKeyHash -> BuiltinData -> V2.ScriptContext -> CompiledCode BuiltinUnit
runValidator genesisUtxo datum redeemer ctx =
  compiledValidator
    `appArg` genesisUtxo
    `appArg` datum
    `appArg` redeemer
    `appArg` ctx
