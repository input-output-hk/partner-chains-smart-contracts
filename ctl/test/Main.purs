module Test.Main (main) where

import Contract.Monad (launchAff_)
import Contract.Prelude (Effect, Maybe(..), Unit, discard, ($), (/\))
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Data.Time.Duration (Milliseconds(..))
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.MerkleTree as MerkleTree
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.UpdateCommitteeHash as UpdateCommitteeHash

main ∷ Effect Unit
main = do
  MerkleTree.test -- Merkletree has its own testing framework atm
  launchAff_ do
    let
      distribute = [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]
        /\ [ BigInt.fromInt 2_000_000_000 ]
      testConfig =
        { slow: Milliseconds 5000.0
        , timeout: Just (Milliseconds 100_000.0) -- plutip can take 15s+ to terminate
        , exit: false
        }
      plutipTest go = runPlutipContract config distribute
        \(alice /\ _bob) → withKeyWallet alice go

    runSpec' testConfig [ consoleReporter ] do
      --    describe "Merkle tree integration tests" do
      --      it "" MerkleTree.test
      describe "testScenarios" do
        it "committeeCandidateValidator" $ plutipTest
          CommitteeCandidateValidator.testScenario
        it "fuelMintingPolicy" $ plutipTest FUELMintingPolicy.testScenario
        it "committeeCandidateValidator" $ plutipTest
          UpdateCommitteeHash.testScenario

-- it "adds 1 and 1" $ 1 + 1 `shouldEqual` 2
-- pending "add some test"
-- pending' "adds 1 and 1" $ 1 + 1 `shouldEqual` 2 -- body will be ignored
