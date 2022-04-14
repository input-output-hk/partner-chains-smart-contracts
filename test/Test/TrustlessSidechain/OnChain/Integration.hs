{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TrustlessSidechain.OnChain.Integration (test) where

import Control.Monad (void)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Plutus.Contract (waitNSlots)
import Test.Plutip.Contract (assertExecution, initAda, withContract)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldSucceed)
import Test.Tasty (TestTree)
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  DeregisterParams (DeregisterParams),
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
  withCluster
    "Plutip integration test"
    [ assertExecution
        "CommitteeCandidateValidator.register"
        (initAda 100 <> initAda 1)
        ( withContract $
            \[PaymentPubKeyHash pkh] -> -- Using a regular signing key instead of an SPO cold key
              ( do
                  let sidechainPubKey = ""
                  CommitteeCandidateValidator.register
                    ( RegisterParams
                        sidechainParams
                        pkh
                        sidechainPubKey
                    )
              )
        )
        [shouldSucceed]
    , assertExecution
        "CommitteeCandidateValidator.deregister"
        (initAda 100 <> initAda 1)
        ( withContract $
            \[PaymentPubKeyHash pkh] -> -- Using a regular signing key instead of an SPO cold key
              ( do
                  let sidechainPubKey = ""
                  CommitteeCandidateValidator.register
                    ( RegisterParams
                        sidechainParams
                        pkh
                        sidechainPubKey
                    )
                  void $ waitNSlots 1
                  CommitteeCandidateValidator.deregister
                    (DeregisterParams sidechainParams pkh)
              )
        )
        [shouldSucceed]
    ]
