module Main where

import Test.Tasty

import Specs.CommitteeCandidateValidator qualified
import Specs.DParameter qualified
import Specs.GovernedMap qualified
import Specs.IlliquidCirculationSupply qualified
import Specs.PermissionedCandidates qualified
import Specs.Reserve qualified
import Specs.Versioning qualified

import Perf.CommitteeCandidateValidator qualified
import Perf.DParameter qualified
import Perf.GovernedMap qualified
import Perf.IlliquidCirculationSupply qualified
import Perf.PermissionedCandidates qualified
import Perf.Reserve qualified
import Perf.Versioning qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "script-spec"
      [ testGroup
          "Script spec tests"
          [ Specs.CommitteeCandidateValidator.validatorTests
          , Specs.DParameter.policyTests
          , Specs.DParameter.validatorTests
          , Specs.GovernedMap.policyTests
          , Specs.GovernedMap.validatorTests
          , Specs.IlliquidCirculationSupply.policyTests
          , Specs.IlliquidCirculationSupply.validatorTests
          , Specs.PermissionedCandidates.policyTests
          , Specs.PermissionedCandidates.validatorTests
          , Specs.Reserve.policyTests
          , Specs.Reserve.validatorTests
          , Specs.Versioning.policyTests
          , Specs.Versioning.validatorTests
          ]
      , testGroup
          "Execution costs"
          [ Perf.CommitteeCandidateValidator.execCosts
          , Perf.DParameter.execCosts
          , Perf.GovernedMap.execCosts
          , Perf.IlliquidCirculationSupply.execCosts
          , Perf.PermissionedCandidates.execCosts
          , Perf.Reserve.execCosts
          , Perf.Versioning.execCosts
          ]
      ]
