module Main where

import Test.Tasty

import Specs.CommitteeCandidateValidator qualified
import Specs.DParameter qualified
import Specs.GovernedMap qualified
import Specs.IlliquidCirculationSupply qualified
import Specs.PermissionedCandidates qualified
import Specs.Reserve qualified
import Specs.Versioning qualified

main :: IO ()
main =
  defaultMain $
    testGroup
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
