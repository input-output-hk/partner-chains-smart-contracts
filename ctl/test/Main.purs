module Test.Main (main) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_)
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (catchError)
import Data.BigInt as BigInt
import Effect.Exception (message)
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.MerkleTree as MerkleTree
import Test.UpdateCommitteeHash as UpdateCommitteeHash

-- Note. it is necessary to be running a `plutip-server` somewhere for this
main ∷ Effect Unit
main = do
  -- Run the merkle tree integration tests
  MerkleTree.test

  -- Run the plutip tests
  launchAff_ do
    let
      distribute = [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]
        /\ [ BigInt.fromInt 2_000_000_000 ]

    runPlutipContract config distribute \(alice /\ bob) → do
      withKeyWallet alice do
        CommitteeCandidateValidator.testScenarioSuccess
        CommitteeCandidateValidator.testScenarioFailure1 # fails
        CommitteeCandidateValidator.testScenarioFailure2 alice bob # fails
        FUELMintingPolicy.testScenario
        UpdateCommitteeHash.testScenario

-- print nicer failing tests that don't have a stack trace and don't halt the program
fails ∷ Contract () Unit → Contract () Unit
fails = flip catchError \e → logInfo' ("Expected failure: " <> message e)
