module Test.TrustlessSidechain.Golden.Versioning where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BC
import PlutusLedgerApi.V1.Data.Value (currencySymbol)
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.Types (
  VersionOracle (..),
  VersionOracleConfig (..),
 )

hexToBSUnsafe :: String -> ByteString
hexToBSUnsafe str =
  let bs = BC.pack str
   in case B16.decode bs of
        Right raw -> raw
        Left e -> error e

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
    { versionOracleCurrencySymbol = currencySymbol (hexToBSUnsafe "726551f3f61ebd8f53198f7c137c646ae0bd57fb180c59759919174d")
    }
