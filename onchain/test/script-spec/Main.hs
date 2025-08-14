module Main where

import Data.String
import GovernedMap qualified
import Test.Tasty
import Prelude

import CommitteeCandidateValidator qualified
import DParameter qualified
import IlliquidCirculationSupply qualified
import PermissionedCandidates qualified
import Reserve qualified
import ScriptCache qualified
import Versioning qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "Script spec tests"
      [ Versioning.policyTests
      , DParameter.policyTests
      , DParameter.validatorTests
      , PermissionedCandidates.policyTests
      , PermissionedCandidates.validatorTests
      , GovernedMap.policyTests
      , GovernedMap.validatorTests
      , IlliquidCirculationSupply.validatorTests
      , CommitteeCandidateValidator.validatorTests
      , Reserve.validatorTests
      , ScriptCache.validatorTests
      ]
