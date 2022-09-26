module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.PoCInlineDatum as PoCInlineDatum
import Test.PoCReferenceInput as PoCReferenceInput
import Test.PoCReferenceScript as PoCReferenceScript

-- import Test.UpdateCommitteeHash as UpdateCommitteeHash

-- Note. it is necessary to be running a `plutip-server` somewhere for this
main ∷ Effect Unit
main = launchAff_ $ do
  let
    distribute = [ BigInt.fromInt 1_000_000_000, BigInt.fromInt 2_000_000_000 ]
      /\ [ BigInt.fromInt 2_000_000_000 ]

  runPlutipContract config distribute \(alice /\ _bob) → do
    withKeyWallet alice $ do
      CommitteeCandidateValidator.testScenario
      FUELMintingPolicy.testScenario

      -- TODO: Fix this.. the CLI merkle tree interface branch fixes this.
      -- UpdateCommitteeHash.testScenario

      PoCInlineDatum.testScenario1
      PoCInlineDatum.testScenario2

      PoCReferenceInput.testScenario1
      PoCReferenceInput.testScenario2

      PoCReferenceScript.testScenario1
      PoCReferenceScript.testScenario2
