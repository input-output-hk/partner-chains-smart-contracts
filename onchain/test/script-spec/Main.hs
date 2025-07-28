module Main where

import Data.String
import Test.Tasty
import Versioning
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "Script spec tests"
      [ testGroup
          "versioning policy (initialize)"
          [ versioningPolicyInitializePassing
          , versioningPolicyInitializeFailing01
          , versioningPolicyInitializeFailing02NoOutput
          , versioningPolicyInitializeFailing02NoDatum
          , versioningPolicyInitializeFailing02InvalidDatum
          , versioningPolicyInitializeFailing03
          ]
      , testGroup
          "versioning policy (mint)"
          [ versioningPolicyMintPassing
          , versioningPolicyMintFailing04
          , versioningPolicyMintFailing05
          , versioningPolicyMintFailing06
          ]
      , testGroup
          "versioning policy (burn)"
          [ versioningPolicyBurnPassing
          , versioningPolicyBurnFailing07
          , versioningPolicyBurnFailing08
          , versioningPolicyBurnFailing09
          ]
      ]
