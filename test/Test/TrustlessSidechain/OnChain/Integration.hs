module Test.TrustlessSidechain.OnChain.Integration (test) where

-- import Control.Monad (void)
-- import Plutus.Contract (waitNSlots)

-- DeregisterParams (DeregisterParams),

import Data.Default (def)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.Contract (ownPaymentPubKeyHash)
import Test.Plutip.Config (PlutipConfig (bpiForceBudget))
import Test.Plutip.Contract (assertExecution, initAda, withContract)
import Test.Plutip.LocalCluster (withConfiguredCluster)
import Test.Plutip.Predicate (shouldSucceed)
import Test.Tasty (TestTree)
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  RegisterParams (RegisterParams),
  SidechainParams (SidechainParams),
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import Prelude

sidechainParams :: SidechainParams
sidechainParams =
  SidechainParams
    { chainId = ""
    , genesisHash = ""
    }

test :: TestTree
test =
  withConfiguredCluster
    (def {bpiForceBudget = Just (8_000_000, 40000)})
    "Plutip integration test"
    [ assertExecution
        "CommitteeCandidateValidator.register"
        (initAda 100)
        ( withContract $
            const
              ( do
                  PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
                  let stakingPkh = pkh
                      sidechainPubKey = ""
                  CommitteeCandidateValidator.register
                    ( RegisterParams
                        sidechainParams
                        stakingPkh
                        sidechainPubKey
                    )
              )
        )
        [shouldSucceed]
        -- , assertExecution
        --     "CommitteeCandidateValidator.deregister"
        --     (initAda 100)
        --     ( withContract $
        --         const
        --           ( do
        --               let stakingPkh = ""
        --                   sidechainPubKey = ""
        --               CommitteeCandidateValidator.register
        --                 ( RegisterParams
        --                     sidechainParams
        --                     stakingPkh
        --                     sidechainPubKey
        --                 )
        --               void $ waitNSlots 1
        --               CommitteeCandidateValidator.deregister
        --                 (DeregisterParams sidechainParams stakingPkh)
        --           )
        --     )
        --     [shouldSucceed]
    ]
