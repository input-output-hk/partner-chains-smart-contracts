module Data.Wrappers (
  productToData2,
  productFromData2,
  productUnsafeFromData2,
  cpsProductFromData3,
  directProductFromData3,
  cpsProductUnsafeFromData3,
  directProductUnsafeFromData3,
) where

import Data.Kind (Type)
import Data.String qualified as HString
import PlutusTx.Builtins (matchList)
import PlutusTx.Builtins.Internal qualified as Unsafe
import TrustlessSidechain.PlutusPrelude hiding (
  productFromData2,
  productToData2,
  productUnsafeFromData2,
 )

{-# INLINE productToData2 #-}
productToData2 ::
  forall (a :: Type) (b :: Type).
  (ToData a, ToData b) =>
  a ->
  b ->
  BuiltinData
productToData2 x y =
  Unsafe.mkList
    ( Unsafe.mkCons
        (toBuiltinData x)
        ( Unsafe.mkCons
            (toBuiltinData y)
            (Unsafe.mkNilData Unsafe.unitval)
        )
    )

{-# INLINE productFromData2 #-}
productFromData2 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (FromData a, FromData b) =>
  (a -> b -> Maybe c) ->
  BuiltinData ->
  Maybe c
productFromData2 f dat =
  Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
  where
    go :: Maybe c
    go =
      let ell0 = Unsafe.unsafeDataAsList dat
       in matchList ell0 Nothing $ \x ell1 ->
            case fromBuiltinData x of
              Nothing -> Nothing
              Just x' -> matchList ell1 Nothing $ \y ell2 ->
                case fromBuiltinData y of
                  Nothing -> Nothing
                  Just y' -> matchList ell2 (f x' y') (\_ _ -> Nothing)

{-# INLINE productUnsafeFromData2 #-}
productUnsafeFromData2 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (UnsafeFromData a, UnsafeFromData b) =>
  (a -> b -> c) ->
  BuiltinData ->
  c
productUnsafeFromData2 f dat =
  let ell0 = Unsafe.unsafeDataAsList dat
      x = unsafeFromBuiltinData (Unsafe.head ell0)
      ell1 = Unsafe.tail ell0
      y = unsafeFromBuiltinData (Unsafe.head ell1)
   in f x y

{-# ANN cpsProductFromData3 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE cpsProductFromData3 #-}
cpsProductFromData3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (FromData a, FromData b, FromData c) =>
  (a -> b -> c -> Maybe d) ->
  BuiltinData ->
  Maybe d
cpsProductFromData3 f dat =
  Unsafe.chooseData dat Nothing Nothing (go (Unsafe.unsafeDataAsList dat)) Nothing Nothing
  where
    go :: Unsafe.BuiltinList BuiltinData -> Maybe d
    go =
      step
        fromBuiltinData
        ( \x ->
            step
              fromBuiltinData
              ( \y ->
                  step fromBuiltinData (\z -> done (f x y z))
              )
        )

{-# INLINE directProductFromData3 #-}
directProductFromData3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (FromData a, FromData b, FromData c) =>
  (a -> b -> c -> Maybe d) ->
  BuiltinData ->
  Maybe d
directProductFromData3 f dat =
  Unsafe.chooseData dat Nothing Nothing go Nothing Nothing
  where
    go :: Maybe d
    go =
      let ell0 = Unsafe.unsafeDataAsList dat
       in matchList ell0 Nothing $ \x ell1 ->
            case fromBuiltinData x of
              Nothing -> Nothing
              Just x' -> matchList ell1 Nothing $ \y ell2 ->
                case fromBuiltinData y of
                  Nothing -> Nothing
                  Just y' -> matchList ell2 Nothing $ \z ell3 ->
                    case fromBuiltinData z of
                      Nothing -> Nothing
                      Just z' -> matchList ell3 (f x' y' z') (\_ _ -> Nothing)

{-# ANN cpsProductUnsafeFromData3 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE cpsProductUnsafeFromData3 #-}
cpsProductUnsafeFromData3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c) =>
  (a -> b -> c -> d) ->
  BuiltinData ->
  d
cpsProductUnsafeFromData3 f dat =
  step'
    unsafeFromBuiltinData
    ( \x ->
        step'
          unsafeFromBuiltinData
          ( \y ->
              done' unsafeFromBuiltinData (\z -> f x y z)
          )
    )
    (Unsafe.unsafeDataAsList dat)

{-# INLINE directProductUnsafeFromData3 #-}
directProductUnsafeFromData3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c) =>
  (a -> b -> c -> d) ->
  BuiltinData ->
  d
directProductUnsafeFromData3 f dat =
  let ell0 = Unsafe.unsafeDataAsList dat
      x = unsafeFromBuiltinData (Unsafe.head ell0)
      ell1 = Unsafe.tail ell1
      y = unsafeFromBuiltinData (Unsafe.head ell1)
      ell2 = Unsafe.tail ell1
      z = unsafeFromBuiltinData (Unsafe.head ell2)
   in f x y z

-- Helpers

step ::
  forall (k :: Type) (r :: Type).
  (BuiltinData -> Maybe k) ->
  (k -> Unsafe.BuiltinList BuiltinData -> Maybe r) ->
  Unsafe.BuiltinList BuiltinData ->
  Maybe r
step f cb ell = matchList ell Nothing $ \x xs ->
  case f x of
    Nothing -> Nothing
    Just x' -> cb x' xs

step' ::
  forall (k :: Type) (r :: Type).
  (BuiltinData -> k) ->
  (k -> Unsafe.BuiltinList BuiltinData -> r) ->
  Unsafe.BuiltinList BuiltinData ->
  r
step' f cb ell =
  let x = f (Unsafe.head ell)
      ell' = Unsafe.tail ell
   in cb x ell'

done :: Maybe d -> Unsafe.BuiltinList BuiltinData -> Maybe d
done res ell = matchList ell res (\_ _ -> Nothing)

done' ::
  forall (k :: Type) (r :: Type).
  (BuiltinData -> k) ->
  (k -> r) ->
  Unsafe.BuiltinList BuiltinData ->
  r
done' f g ell = g (f (Unsafe.head ell))
