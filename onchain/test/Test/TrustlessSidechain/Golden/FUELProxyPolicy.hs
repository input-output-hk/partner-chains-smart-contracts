module Test.TrustlessSidechain.Golden.FUELProxyPolicy where

import Data.String
import GHC.Num (fromInteger)
import Test.Tasty (TestTree, testGroup)
import Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest)
import TrustlessSidechain.FUELProxyPolicy (
  FuelProxyRedeemer (
    FuelProxyBurn,
    FuelProxyMint,
    recipient,
    version
  ),
 )

tests :: TestTree
tests =
  testGroup
    "Golden tests for FUELProxyPolicy module"
    [ dataEncoderGoldenTest "FuelProxyRedeemer1" sampleFuelProxyRedeemer1
    , dataEncoderGoldenTest "FuelProxyRedeemer2" sampleFuelProxyRedeemer2
    ]

sampleFuelProxyRedeemer1 :: FuelProxyRedeemer
sampleFuelProxyRedeemer1 =
  FuelProxyMint
    { version = 15
    }

sampleFuelProxyRedeemer2 :: FuelProxyRedeemer
sampleFuelProxyRedeemer2 =
  FuelProxyBurn
    { version = 15
    , recipient = "4f2d6145e1700ad11dc074cad9f4194cc53b0dbab6bd25dfea6c501a"
    }
