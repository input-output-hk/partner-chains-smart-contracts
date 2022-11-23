module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.InitSidechain as InitSidechain
import Test.MPTRoot as MPTRoot
import Test.MerkleRootChaining as MerkleRootChaining
import Test.MerkleTree as MerkleTree
import Test.Options as Options
import Test.PoCECDSA as PoCECDSA
import Test.PoCInlineDatum as PoCInlineDatum
import Test.PoCReferenceInput as PoCReferenceInput
import Test.PoCReferenceScript as PoCReferenceScript
import Test.PoCSerialiseData as PoCSerialiseData
import Test.UpdateCommitteeHash as UpdateCommitteeHash
import Test.Utils as Test.Utils

-- Note. it is necessary to be running a `plutip-server` somewhere for this
main ∷ Effect Unit
main = do
  -- Run the merkle tree integration tests
  MerkleTree.test

  -- Run the Options tests.
  Options.test

  -- Run the plutip tests
  launchAff_ do
    -- Default ada distribution
    let
      distribute =
        [ BigInt.fromInt 2_000_000_000
        , BigInt.fromInt 2_000_000_000
        , BigInt.fromInt 2_000_000_000
        ]
          /\ [ BigInt.fromInt 2_000_000_000 ]

    -- Run the plutip tests
    runPlutipContract config distribute \(alice /\ bob) → do
      withKeyWallet alice do
        CommitteeCandidateValidator.testScenarioSuccess
        CommitteeCandidateValidator.testScenarioFailure1 # Test.Utils.fails
        CommitteeCandidateValidator.testScenarioFailure2 alice bob #
          Test.Utils.fails

        FUELMintingPolicy.testScenario

        UpdateCommitteeHash.testScenario1
        UpdateCommitteeHash.testScenario2
        UpdateCommitteeHash.testScenario3
        UpdateCommitteeHash.testScenario4

        MPTRoot.testScenario1
        MPTRoot.testScenario2

        MerkleRootChaining.testScenario1
        MerkleRootChaining.testScenario2

        InitSidechain.testScenario1
        InitSidechain.testScenario2
        InitSidechain.testScenario3 alice bob

    -- Run the plutip tests for the proof of concept tests (note we run these
    -- separately from the actual sidechain tests.)
    runPlutipContract config distribute \(alice /\ _bob) → do
      withKeyWallet alice do
        PoCInlineDatum.testScenario1
        PoCInlineDatum.testScenario2

        PoCReferenceInput.testScenario1
        PoCReferenceInput.testScenario2

        PoCReferenceScript.testScenario1
        PoCReferenceScript.testScenario2

        PoCSerialiseData.testScenario1
        PoCSerialiseData.testScenario2

        PoCECDSA.testScenario
