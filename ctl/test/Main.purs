module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.MerkleTree as MerkleTree
import Test.PoCInlineDatum as PoCInlineDatum
import Test.PoCReferenceInput as PoCReferenceInput
import Test.PoCReferenceScript as PoCReferenceScript
import Test.UpdateCommitteeHash as UpdateCommitteeHash

-- Note. it is necessary to be running a `plutip-server` somewhere for this
main ∷ Effect Unit
main = do
  -- Run the merkle tree integration tests
  MerkleTree.test

  -- Run the plutip tests
  launchAff_ do
    -- Default ada distribution
    let
      distribute = [ BigInt.fromInt 2_000_000_000, BigInt.fromInt 2_000_000_000 ]
        /\ [ BigInt.fromInt 2_000_000_000 ]

    -- Run the plutip tests
    runPlutipContract config distribute \(alice /\ _bob) → do
      withKeyWallet alice $ do
        CommitteeCandidateValidator.testScenario
        FUELMintingPolicy.testScenario

        UpdateCommitteeHash.testScenario

    -- Run the proof of concept tests (note that we run each test separately from
    -- the other tests
    let
      pocTestScenarios =
        [ PoCInlineDatum.testScenario1
        , PoCInlineDatum.testScenario2

        , PoCReferenceInput.testScenario1
        , PoCReferenceInput.testScenario2

        , PoCReferenceScript.testScenario1
        , PoCReferenceScript.testScenario2
        ]

    for pocTestScenarios
      \scenario → runPlutipContract config distribute
        \(alice /\ _bob) → withKeyWallet alice scenario
