module TrustlessSidechain.Utils.Asset
  ( emptyAssetName
  , unsafeMkAssetName
  , getScriptHash
  , currencySymbolToHex
  , singletonFromAsset
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types.Asset (Asset(..))
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.AssetName (AssetName, mkAssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.Value as Value
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.Value (CurrencySymbol, Value)
import Data.ByteArray (byteArrayFromAscii)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

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

singletonFromAsset ∷ Asset → BigNum → Value
singletonFromAsset AdaAsset amount = Value.mkValue (wrap amount)
  MultiAsset.empty
singletonFromAsset (Asset cs tn) amount = Value.singleton cs tn amount
