
module Test.TrustlessSidechain.OnChain.Integration (test) where

import Control.Exception (ErrorCall, Exception (fromException))
import Control.Lens ((^.))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Ledger (CardanoTx, ChainIndexTxOut, PaymentPubKeyHash, TxOutRef, Value, ciTxOutValue, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (MkTxError (OwnPubKeyMissing))
import Ledger.Constraints qualified as Constraints
import Plutus.Contract (Contract, ContractError (ConstraintResolutionContractError), submitTx, utxosAt)
import Plutus.Contract qualified as Contract
import Plutus.PAB.Effects.Contract.Builtin (EmptySchema)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)
import Test.Plutip.Config (PlutipConfig (bpiForceBudget))
import Test.Plutip.Contract (ValueOrdering (VLt), assertExecution, initAda, initAndAssertAda, initAndAssertAdaWith, initLovelace, withContract, withContractAs)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (errorSatisfies, failReasonSatisfies, shouldFail, shouldSucceed, shouldThrow, shouldYield, stateIs, stateSatisfies, yieldSatisfies)
import Test.Plutip.Predicate qualified as Predicate
import Test.Tasty (TestTree)
import Text.Printf (printf)
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator


sidechainParams :: CommitteeCandidateValidator.SidechainParams
sidechainParams =
  SidechainParams
    { chainId :: BuiltinInteger
    , genesisHash :: BuiltinByteString
    }

test :: TestTree
test =
  withCluster
    "Plutip integration test"
    [ assertExecution
        "CommitteeCandidateValidator.register"
        (initAda 100)
        (withContract $ const (
            CommitteeCandidateValidator.register 
              (RegisterParams sidechainParams "")))
        [ shouldSucceed ]
    , assertExecution
        "CommitteeCandidateValidator.deregister"
        (initAda 100)
        (withContract $ const (do
            CommitteeCandidateValidator.register (RegisterParams sidechainParams "")
            void $ awaitNSlots 1
            CommitteeCandidateValidator.unregister (DeregisterParams sidechainParams)
            )
        )
        [ shouldSucceed ]
    ]

