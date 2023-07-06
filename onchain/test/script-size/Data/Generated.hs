{-# LANGUAGE TemplateHaskell #-}

module Data.Generated (Foo (..), Bar (..)) where

import Data.Wrappers (productFromData2, productToData2, productUnsafeFromData2)
import Ledger.Value (CurrencySymbol)
import PlutusTx (makeIsDataIndexed)
import TrustlessSidechain.PlutusPrelude hiding (
  productFromData2,
  productToData2,
  productUnsafeFromData2,
 )
import TrustlessSidechain.Types (SidechainParams)

data Foo = Foo
  { tcs :: CurrencySymbol
  , sp :: SidechainParams
  , kcs :: CurrencySymbol
  }

makeIsDataIndexed ''Foo [('Foo, 0)]

data Bar = Bar Integer BuiltinByteString

instance ToData Bar where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Bar x y) = productToData2 x y

instance FromData Bar where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 (\x y -> Just (Bar x y))

instance UnsafeFromData Bar where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 Bar
