module Main where

import Test.Tasty

import CommitteeCandidateValidator qualified
import DParameter qualified
import GovernedMap qualified
import IlliquidCirculationSupply qualified
import PermissionedCandidates qualified
import Reserve qualified
import Versioning qualified

main :: IO ()
main =
  defaultMain do
    testGroup
      "Execution costs"
      [ CommitteeCandidateValidator.execCosts
      , DParameter.execCosts
      , GovernedMap.execCosts
      , IlliquidCirculationSupply.execCosts
      , PermissionedCandidates.execCosts
      , Reserve.execCosts
      , Versioning.execCosts
      ]
