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
import Test.InitSidechain as InitSidechain
import Test.MerkleTree as MerkleTree
import Test.PoCInlineDatum as PoCInlineDatum
import Test.PoCReferenceInput as PoCReferenceInput
import Test.PoCReferenceScript as PoCReferenceScript
import Test.PoCSerialiseData as PoCSerialiseData
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
    runPlutipContract config distribute \(alice /\ bob) → do
      withKeyWallet alice do
        CommitteeCandidateValidator.testScenarioSuccess
        CommitteeCandidateValidator.testScenarioFailure1 # fails
        CommitteeCandidateValidator.testScenarioFailure2 alice bob # fails

        FUELMintingPolicy.testScenario

        UpdateCommitteeHash.testScenario

        InitSidechain.testScenario1
        InitSidechain.testScenario2 alice bob

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

-- print nicer failing tests that don't have a stack trace and don't halt the program
fails ∷ Contract () Unit → Contract () Unit
fails = flip catchError \e → logInfo' ("Expected failure: " <> message e)