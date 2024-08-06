module Test.TrustlessSidechain.GoldenTest (dataEncoderGoldenTest) where

import TrustlessSidechain.HaskellPrelude

import Data.ByteString.Lazy (fromStrict)
import Data.String qualified as HString
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import TrustlessSidechain.OffChain (encodeHexBuiltinBS)

-- | Creating a test group with two golden tests:
-- - encoding data using `toBuiltinData`
-- - serialising BuiltinData to CBOR
--
-- Results of the tests are compared to the files under ./test/golden/*.golden
-- If no file exists for the given data type, a new one will be created automatically
dataEncoderGoldenTest :: (ToData a) => HString.String -> a -> TestTree
dataEncoderGoldenTest name sampleData =
  let builtinData = toBuiltinData sampleData
      plutusDataBS = fromStrict $ encodeUtf8 $ Text.pack $ show builtinData
      cborBS = fromStrict $ encodeHexBuiltinBS $ Builtins.serialiseData builtinData
   in testGroup
        ("Serialising " <> name)
        [ goldenVsString "IsData encoding" ("./test/golden/" <> name <> "-isdata.golden") $ pure plutusDataBS
        , goldenVsString "CBOR encoding" ("./test/golden/" <> name <> "-cbor.golden") $ pure cborBS
        ]
