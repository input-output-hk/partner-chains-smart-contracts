{-# LANGUAGE RecordWildCards #-}

module Data.Handwritten (Foo (..)) where

import Ledger.Value (CurrencySymbol)
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Builtins (chooseData, mkList, unsafeDataAsList)
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (SidechainParams)

data Foo = Foo
  { tcs :: CurrencySymbol
  , sp :: SidechainParams
  , kcs :: CurrencySymbol
  }

instance ToData Foo where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Foo {..}) =
    mkList
      [ toBuiltinData tcs
      , toBuiltinData sp
      , toBuiltinData kcs
      ]

instance FromData Foo where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat =
    chooseData
      dat
      Nothing
      Nothing
      go
      Nothing
      Nothing
    where
      go :: Maybe Foo
      go = case unsafeDataAsList dat of
        [x, y, z] ->
          Foo <$> fromBuiltinData x
            <*> fromBuiltinData y
            <*> fromBuiltinData z
        _ -> Nothing

instance UnsafeFromData Foo where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat = case unsafeDataAsList dat of
    [x, y, z] ->
      Foo
        (unsafeFromBuiltinData x)
        (unsafeFromBuiltinData y)
        (unsafeFromBuiltinData z)
    _ -> error ()
