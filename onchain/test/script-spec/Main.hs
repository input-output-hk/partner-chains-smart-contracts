module Main where

import Data.String
import GovernedMap qualified
import Test.Tasty
import Prelude

import DParameter qualified
import IlliquidCirculationSupply qualified
import PermissionedCandidates qualified
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
      ]
