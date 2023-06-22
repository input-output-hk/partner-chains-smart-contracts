{-# LANGUAGE RecordWildCards #-}

module Data.Handwritten (Foo (..)) where

import Ledger.Value (CurrencySymbol)
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Builtins (matchList)
import PlutusTx.Builtins.Internal (
  chooseData,
  mkCons,
  mkList,
  mkNilData,
  unitval,
  unsafeDataAsList,
 )
import PlutusTx.Builtins.Internal qualified as Unsafe
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
      ( mkCons
          (toBuiltinData tcs)
          ( mkCons
              (toBuiltinData sp)
              ( mkCons
                  (toBuiltinData kcs)
                  (mkNilData unitval)
              )
          )
      )

instance FromData Foo where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe Foo
      go =
        let ell = unsafeDataAsList dat
         in matchList ell Nothing $ \tcs ell' ->
              case fromBuiltinData tcs of
                Nothing -> Nothing
                Just tcs' -> matchList ell' Nothing $ \sp ell'' ->
                  case fromBuiltinData sp of
                    Nothing -> Nothing
                    Just sp' -> matchList ell'' Nothing $ \kcs ell''' ->
                      case fromBuiltinData kcs of
                        Nothing -> Nothing
                        Just kcs' ->
                          matchList
                            ell'''
                            (Just (Foo tcs' sp' kcs'))
                            (\_ _ -> Nothing)

instance UnsafeFromData Foo where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell = unsafeDataAsList dat
        tcs = unsafeFromBuiltinData (Unsafe.head ell)
        ell' = Unsafe.tail ell
        sp = unsafeFromBuiltinData (Unsafe.head ell)
        ell'' = Unsafe.tail ell'
        kcs = unsafeFromBuiltinData (Unsafe.head ell'')
     in Foo tcs sp kcs
