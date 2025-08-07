module Main where

import Data.String
import Test.Tasty
import Prelude

import DParameter qualified
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
      ]
