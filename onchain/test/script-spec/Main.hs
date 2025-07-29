module Main where

import Data.String
import Test.Tasty
import Versioning qualified
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "Script spec tests"
      [ Versioning.tests
      ]
