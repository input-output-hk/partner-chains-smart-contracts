module TrustlessSidechain.Utils.Asset
  ( emptyAssetName
  , unsafeMkAssetName
  , getScriptHash
  , currencySymbolToHex
  , singletonFromAsset
  ) where

import Contract.Prelude

import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.Asset (Asset(..))
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe, byteArrayToHex)
import Data.ByteArray (byteArrayFromAscii)
import Cardano.Types.BigNum (BigNum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Contract.Value (CurrencySymbol, Value)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.Value as Value
import Cardano.AsCbor (encodeCbor)

emptyAssetName ∷ AssetName
emptyAssetName =
  (unsafePartial $ fromJust $ mkAssetName $ hexToByteArrayUnsafe "")

mkAssetName' ∷ String → Maybe AssetName
mkAssetName' name = do
  byteName ← byteArrayFromAscii name
  mkAssetName byteName

unsafeMkAssetName ∷ String → AssetName
unsafeMkAssetName name = unsafePartial $ fromJust $ mkAssetName' name

getScriptHash ∷ AssetClass → ScriptHash
getScriptHash (AssetClass sc _) = sc

-- | Convert a currency symbol to a hex string
currencySymbolToHex ∷ CurrencySymbol → String
currencySymbolToHex cs = byteArrayToHex $ unwrap $ encodeCbor cs

singletonFromAsset :: Asset -> BigNum -> Value
singletonFromAsset AdaAsset amount = Value.mkValue (wrap amount) MultiAsset.empty
singletonFromAsset (Asset cs tn) amount = Value.singleton cs tn amount