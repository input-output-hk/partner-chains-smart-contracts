module Main where

import GovernedMap qualified
import Test.Tasty

import CommitteeCandidateValidator qualified
import DParameter qualified
import IlliquidCirculationSupply qualified
import OnlyMintMintingPolicy qualified
import PermissionedCandidates qualified
import Reserve qualified
import ScriptCache qualified
import Versioning qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "Script spec tests"
      [ CommitteeCandidateValidator.validatorTests
      , DParameter.policyTests
      , DParameter.validatorTests
      , GovernedMap.policyTests
      , GovernedMap.validatorTests
      , IlliquidCirculationSupply.policyTests
      , IlliquidCirculationSupply.validatorTests
      , OnlyMintMintingPolicy.policyTests
      , PermissionedCandidates.policyTests
      , PermissionedCandidates.validatorTests
      , Reserve.policyTests
      , Reserve.validatorTests
      , ScriptCache.validatorTests
      , Versioning.policyTests
      , Versioning.validatorTests
      ]
