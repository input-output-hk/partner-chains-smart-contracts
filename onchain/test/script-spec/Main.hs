module Main where

import Data.String
import ScriptSpecUtils
import Test.Tasty
import TrustlessSidechain.AlwaysFailingScripts
import TrustlessSidechain.AlwaysPassingScripts
import Versioning
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "Script spec tests"
      [ myTest
      , myTest2
      , testGroup
          "versioning policy (initialize)"
          [ versioningPolicyInitializeTestPassing
          , versioningPolicyInitializeTestFailing01
          , versioningPolicyInitializeTestFailing02
          , versioningPolicyInitializeTestFailing03
          ]
      , testGroup
          "versioning policy (mint)"
          [ versioningPolicyMintTestPassing
          , versioningPolicyMintTestFailing04
          , versioningPolicyMintTestFailing05
          , versioningPolicyMintTestFailing06
          ]
      , testGroup
          "versioning policy (burn)"
          [ versioningPolicyBurnTestPassing
          -- , versioningPolicyBurnTestFailing07
          -- , versioningPolicyBurnTestFailing08
          -- , versioningPolicyBurnTestFailing09
          ]
      ]

myTest :: TestTree
myTest = expectFail "always failing should fail" $ mkAlwaysFailingValidatorUntyped builtinDummy builtinDummy builtinDummy builtinDummy

myTest2 :: TestTree
myTest2 = expectSuccess "always passing should pass" $ mkAlwaysPassingValidatorUntyped builtinDummy builtinDummy builtinDummy builtinDummy
