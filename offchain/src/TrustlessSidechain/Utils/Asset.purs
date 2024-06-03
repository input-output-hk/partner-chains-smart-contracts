module TrustlessSidechain.Utils.Asset
    ( emptyAssetName
    , unsafeMkAssetName
    , getScriptHash
    ) where

import Contract.Prelude
import Partial.Unsafe (unsafePartial)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Data.Maybe (fromJust)
import Data.ByteArray (byteArrayFromAscii)
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.ScriptHash (ScriptHash)

emptyAssetName :: AssetName
emptyAssetName =
      (unsafePartial $ fromJust $ mkAssetName $ hexToByteArrayUnsafe "")




mkAssetName' :: String -> Maybe AssetName
mkAssetName' name = do
    byteName <- byteArrayFromAscii name
    mkAssetName byteName

unsafeMkAssetName :: String -> AssetName
unsafeMkAssetName name = unsafePartial $ fromJust $ mkAssetName' name

getScriptHash :: AssetClass -> ScriptHash
getScriptHash (AssetClass sc _) = sc