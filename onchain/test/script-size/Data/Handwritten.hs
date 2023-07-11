{-# LANGUAGE RecordWildCards #-}

module Data.Handwritten (
  Foo (..),
  Bar (..),
  Baz (..),
  pairToData,
  pairFromData,
  pairUnsafeFromData,
  listToData,
  listFromData,
  listUnsafeFromData,
) where

import Data.Kind (Type)
import Data.Wrappers (
  directProductFromData3,
  directProductToData3,
  directProductUnsafeFromData3,
 )
import Ledger.Value (CurrencySymbol)
import PlutusTx.Builtins (matchList)
import PlutusTx.Builtins.Internal (
  BuiltinList,
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
        sp = unsafeFromBuiltinData (Unsafe.head ell')
        ell'' = Unsafe.tail ell'
        kcs = unsafeFromBuiltinData (Unsafe.head ell'')
     in Foo tcs sp kcs

{-# INLINE pairToData #-}
pairToData ::
  forall (a :: Type) (b :: Type).
  (ToData a, ToData b) =>
  (a, b) ->
  BuiltinData
pairToData (x, y) =
  Unsafe.mkList
    ( Unsafe.mkCons
        (toBuiltinData x)
        (Unsafe.mkCons (toBuiltinData y) (Unsafe.mkNilData Unsafe.unitval))
    )

{-# INLINE pairFromData #-}
pairFromData ::
  forall (a :: Type) (b :: Type).
  (FromData a, FromData b) =>
  BuiltinData ->
  Maybe (a, b)
pairFromData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
  where
    go :: Maybe (a, b)
    go =
      let ell0 = unsafeDataAsList dat
       in matchList ell0 Nothing $ \x ell1 ->
            case fromBuiltinData x of
              Nothing -> Nothing
              Just x' -> matchList ell1 Nothing $ \y ell2 ->
                case fromBuiltinData y of
                  Nothing -> Nothing
                  Just y' ->
                    matchList
                      ell2
                      (Just (x', y'))
                      (\_ _ -> Nothing)

{-# INLINE pairUnsafeFromData #-}
pairUnsafeFromData ::
  forall (a :: Type) (b :: Type).
  (UnsafeFromData a, UnsafeFromData b) =>
  BuiltinData ->
  (a, b)
pairUnsafeFromData dat =
  let ell0 = unsafeDataAsList dat
      x = unsafeFromBuiltinData (Unsafe.head ell0)
      ell1 = Unsafe.tail ell0
      y = unsafeFromBuiltinData (Unsafe.head ell1)
   in (x, y)

{-# INLINE listToData #-}
listToData ::
  forall (a :: Type).
  (ToData a) =>
  [a] ->
  BuiltinData
listToData ell = Unsafe.mkList (go ell)
  where
    go :: [a] -> BuiltinList BuiltinData
    go = \case
      [] -> Unsafe.mkNilData Unsafe.unitval
      (x : xs) -> Unsafe.mkCons (toBuiltinData x) (go xs)

{-# INLINE listFromData #-}
listFromData ::
  forall (a :: Type).
  (FromData a) =>
  BuiltinData ->
  Maybe [a]
listFromData dat = Unsafe.chooseData dat Nothing Nothing (go (unsafeDataAsList dat)) Nothing Nothing
  where
    go :: BuiltinList BuiltinData -> Maybe [a]
    go ell = matchList ell (Just []) $ \x xs ->
      case fromBuiltinData x of
        Nothing -> Nothing
        Just x' -> case go xs of
          Nothing -> Nothing
          Just xs' -> Just (x' : xs')

{-# INLINE listUnsafeFromData #-}
listUnsafeFromData ::
  forall (a :: Type).
  (UnsafeFromData a) =>
  BuiltinData ->
  [a]
listUnsafeFromData dat = go (unsafeDataAsList dat)
  where
    go :: BuiltinList BuiltinData -> [a]
    go ell = matchList ell [] $ \x xs ->
      unsafeFromBuiltinData x : go xs

data Bar = Bar Integer BuiltinByteString

instance ToData Bar where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Bar x y) =
    Unsafe.mkList
      ( Unsafe.mkCons
          (toBuiltinData x)
          ( Unsafe.mkCons
              (toBuiltinData y)
              (Unsafe.mkNilData Unsafe.unitval)
          )
      )

instance FromData Bar where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
    where
      go :: Maybe Bar
      go =
        let ell0 = Unsafe.unsafeDataAsList dat
         in matchList ell0 Nothing $ \x ell1 ->
              case fromBuiltinData x of
                Nothing -> Nothing
                Just x' -> matchList ell1 Nothing $ \y ell2 ->
                  case fromBuiltinData y of
                    Nothing -> Nothing
                    Just y' -> matchList ell2 (Just (Bar x' y')) (\_ _ -> Nothing)

instance UnsafeFromData Bar where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let ell0 = Unsafe.unsafeDataAsList dat
        x = unsafeFromBuiltinData (Unsafe.head ell0)
        ell1 = Unsafe.tail ell0
        y = unsafeFromBuiltinData (Unsafe.head ell1)
     in Bar x y

data Baz = Baz Integer BuiltinByteString Integer

instance ToData Baz where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Baz x y z) = directProductToData3 x y z

instance FromData Baz where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = directProductFromData3 (\x y z -> Just (Baz x y z))

instance UnsafeFromData Baz where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = directProductUnsafeFromData3 Baz
