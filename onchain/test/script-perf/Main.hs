module Main where

import Test.Tasty
import Prelude

import Perf.CommitteeCandidateValidator qualified
import Perf.DParameter qualified
import Perf.GovernedMap qualified
import Perf.IlliquidCirculationSupply qualified
import Perf.PermissionedCandidates qualified
import Perf.Reserve qualified
import Perf.Versioning qualified

import Size (sizeTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Performance"
      [ testGroup
          "Execution costs"
          [ Perf.CommitteeCandidateValidator.execCosts
          , Perf.DParameter.execCosts
          , Perf.GovernedMap.execCosts
          , Perf.IlliquidCirculationSupply.execCosts
          , Perf.PermissionedCandidates.execCosts
          , Perf.Reserve.execCosts
          , Perf.Versioning.execCosts
          ]
      , sizeTests
      ]
