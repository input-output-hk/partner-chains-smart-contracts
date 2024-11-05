module Test.TrustlessSidechain.Golden.Versioning where

import Data.String
import GHC.Num (fromInteger)
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.Versioning (
  VersionOracle (..),
  VersionOracleConfig (..),
 )

tests :: TestTree
tests =
  testGroup
    "Golden tests for Versioning module"
    [ dataEncoderGoldenTest "VersionOracle" sampleVersionOracle
    , dataEncoderGoldenTest "VersionOracleConfig" sampleVersionOracleConfig
    ]

sampleVersionOracle :: VersionOracle
sampleVersionOracle =
  VersionOracle
    { scriptId = 1567894
    }

sampleVersionOracleConfig :: VersionOracleConfig
sampleVersionOracleConfig =
  VersionOracleConfig
    { versionOracleCurrencySymbol = "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d"
    }
