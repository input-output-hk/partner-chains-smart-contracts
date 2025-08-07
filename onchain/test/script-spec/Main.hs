module Main where

import DParameter qualified
import Data.String
import Test.Tasty
import Versioning qualified
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "Script spec tests"
      [ Versioning.policyTests
      , DParameter.policyTests
      , DParameter.validatorTests
      ]
