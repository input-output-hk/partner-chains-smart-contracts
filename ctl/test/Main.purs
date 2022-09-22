module Test.Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Test.Plutip (runPlutipContract)
import Contract.Wallet (withKeyWallet)
import Data.BigInt as BigInt
import Test.CommitteeCandidateValidator as CommitteeCandidateValidator
import Test.Config (config)
import Test.FUELMintingPolicy as FUELMintingPolicy
import Test.UpdateCommitteeHash as UpdateCommitteeHash

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
      UpdateCommitteeHash.testScenario
