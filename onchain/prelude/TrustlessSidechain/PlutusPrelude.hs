module TrustlessSidechain.PlutusPrelude (
  module PlutusTx.Prelude,
  GHC.IsString (..),
  PlutusTx.ToData (..),
  PlutusTx.FromData (..),
  PlutusTx.UnsafeFromData (..),
  HaskellPrelude.fromInteger,
  ifThenElse,

  -- * Helpers for Data encoding

  -- ** ToData
  productToData2,
  productToData3,
  productToData4,
  productToData5,
  productToData6,

  -- ** FromData
  productFromData2,
  productFromData2',
  productFromData3,
  productFromData3',
  productFromData4,
  productFromData4',
  productFromData5,
  productFromData5',
  productFromData6,
  productFromData6',

  -- ** UnsafeFromData
  productUnsafeFromData2,
  productUnsafeFromData3,
  productUnsafeFromData4,
  productUnsafeFromData5,
  productUnsafeFromData6,
) where

import Data.Kind (Type)
import Data.String qualified as HString
import GHC.Exts (fromString)
import GHC.Exts qualified as GHC
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Builtins (matchList)
import PlutusTx.Builtins.Internal qualified as Unsafe
import PlutusTx.Prelude hiding (fromInteger, toList)
import Prelude qualified as HaskellPrelude

{-# INLINE ifThenElse #-}
ifThenElse :: forall (a :: Type). Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

-- | @since Unreleased
{-# INLINE productToData2 #-}
productToData2 ::
  forall (a :: Type) (b :: Type).
  (ToData a, ToData b) =>
  a ->
  b ->
  BuiltinData
productToData2 x y = Unsafe.mkList go
  where
    go :: Unsafe.BuiltinList BuiltinData
    go = step1 toBuiltinData x (done1 toBuiltinData y)

-- | @since Unreleased
{-# INLINE productToData3 #-}
productToData3 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (ToData a, ToData b, ToData c) =>
  a ->
  b ->
  c ->
  BuiltinData
productToData3 x y z = Unsafe.mkList go
  where
    go :: Unsafe.BuiltinList BuiltinData
    go =
      step1
        toBuiltinData
        x
        ( step1
            toBuiltinData
            y
            ( done1 toBuiltinData z
            )
        )

-- | @since Unreleased
{-# INLINE productToData4 #-}
productToData4 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (ToData a, ToData b, ToData c, ToData d) =>
  a ->
  b ->
  c ->
  d ->
  BuiltinData
productToData4 x1 x2 x3 x4 = Unsafe.mkList go
  where
    go :: Unsafe.BuiltinList BuiltinData
    go =
      step1
        toBuiltinData
        x1
        ( step1
            toBuiltinData
            x2
            ( step1
                toBuiltinData
                x3
                ( done1 toBuiltinData x4
                )
            )
        )

-- | @since Unreleased
{-# INLINE productToData5 #-}
productToData5 ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type).
  (ToData a, ToData b, ToData c, ToData d, ToData e) =>
  a ->
  b ->
  c ->
  d ->
  e ->
  BuiltinData
productToData5 x1 x2 x3 x4 x5 = Unsafe.mkList go
  where
    go :: Unsafe.BuiltinList BuiltinData
    go =
      step1
        toBuiltinData
        x1
        ( step1
            toBuiltinData
            x2
            ( step1
                toBuiltinData
                x3
                ( step1
                    toBuiltinData
                    x4
                    ( done1 toBuiltinData x5
                    )
                )
            )
        )

-- | @since Unreleased
{-# INLINE productToData6 #-}
productToData6 ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type).
  (ToData a, ToData b, ToData c, ToData d, ToData e, ToData f) =>
  a ->
  b ->
  c ->
  d ->
  e ->
  f ->
  BuiltinData
productToData6 x1 x2 x3 x4 x5 x6 = Unsafe.mkList go
  where
    go :: Unsafe.BuiltinList BuiltinData
    go =
      step1
        toBuiltinData
        x1
        ( step1
            toBuiltinData
            x2
            ( step1
                toBuiltinData
                x3
                ( step1
                    toBuiltinData
                    x4
                    ( step1
                        toBuiltinData
                        x5
                        ( done1 toBuiltinData x6
                        )
                    )
                )
            )
        )

-- Note from Koz (7/07/23): We disable the 'avoid lambda' hint from HLint for
-- many definitions here, as the cost of function composition makes these
-- routines larger than they would otherwise be, and HLint's suggestions
-- involve use of function composition. In Haskell, this would make perfect
-- sense, but not in Plutus, due to having to pay for it where we'd rather not.

-- | @since Unreleased
{-# ANN productFromData2 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData2 #-}
productFromData2 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (FromData a, FromData b) =>
  (a -> b -> c) ->
  BuiltinData ->
  Maybe c
productFromData2 f = productFromData2' (\x y -> Just (f x y))

-- | @since Unreleased
{-# ANN productFromData2' ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData2' #-}
productFromData2' ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (FromData a, FromData b) =>
  (a -> b -> Maybe c) ->
  BuiltinData ->
  Maybe c
productFromData2' f dat =
  Unsafe.chooseData dat Nothing Nothing (go (Unsafe.unsafeDataAsList dat)) Nothing Nothing
  where
    go :: Unsafe.BuiltinList BuiltinData -> Maybe c
    go =
      step
        fromBuiltinData
        ( \x ->
            step
              fromBuiltinData
              ( \y ->
                  done (f x y)
              )
        )

-- | @Since Unreleased
{-# ANN productFromData3 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData3 #-}
productFromData3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (FromData a, FromData b, FromData c) =>
  (a -> b -> c -> d) ->
  BuiltinData ->
  Maybe d
productFromData3 f = productFromData3' (\x y z -> Just (f x y z))

-- | @Since Unreleased
{-# ANN productFromData3' ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData3' #-}
productFromData3' ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (FromData a, FromData b, FromData c) =>
  (a -> b -> c -> Maybe d) ->
  BuiltinData ->
  Maybe d
productFromData3' f dat =
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
                  step
                    fromBuiltinData
                    ( \z ->
                        done (f x y z)
                    )
              )
        )

-- | @Since Unreleased
{-# ANN productFromData4 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData4 #-}
productFromData4 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type).
  (FromData a, FromData b, FromData c, FromData d) =>
  (a -> b -> c -> d -> e) ->
  BuiltinData ->
  Maybe e
productFromData4 f = productFromData4' (\x1 x2 x3 x4 -> Just (f x1 x2 x3 x4))

-- | @Since Unreleased
{-# ANN productFromData4' ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData4' #-}
productFromData4' ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type).
  (FromData a, FromData b, FromData c, FromData d) =>
  (a -> b -> c -> d -> Maybe e) ->
  BuiltinData ->
  Maybe e
productFromData4' f dat =
  Unsafe.chooseData dat Nothing Nothing (go (Unsafe.unsafeDataAsList dat)) Nothing Nothing
  where
    go :: Unsafe.BuiltinList BuiltinData -> Maybe e
    go =
      step
        fromBuiltinData
        ( \x1 ->
            step
              fromBuiltinData
              ( \x2 ->
                  step
                    fromBuiltinData
                    ( \x3 ->
                        step
                          fromBuiltinData
                          ( \x4 ->
                              done (f x1 x2 x3 x4)
                          )
                    )
              )
        )

-- | @Since Unreleased
{-# ANN productFromData5 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData5 #-}
productFromData5 ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type).
  ( FromData a
  , FromData b
  , FromData c
  , FromData d
  , FromData e
  ) =>
  (a -> b -> c -> d -> e -> f) ->
  BuiltinData ->
  Maybe f
productFromData5 f =
  productFromData5'
    (\x1 x2 x3 x4 x5 -> Just (f x1 x2 x3 x4 x5))

-- | @Since Unreleased
{-# ANN productFromData5' ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData5' #-}
productFromData5' ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type).
  ( FromData a
  , FromData b
  , FromData c
  , FromData d
  , FromData e
  ) =>
  (a -> b -> c -> d -> e -> Maybe f) ->
  BuiltinData ->
  Maybe f
productFromData5' f dat =
  Unsafe.chooseData dat Nothing Nothing (go (Unsafe.unsafeDataAsList dat)) Nothing Nothing
  where
    go :: Unsafe.BuiltinList BuiltinData -> Maybe f
    go =
      step
        fromBuiltinData
        ( \x1 ->
            step
              fromBuiltinData
              ( \x2 ->
                  step
                    fromBuiltinData
                    ( \x3 ->
                        step
                          fromBuiltinData
                          ( \x4 ->
                              step
                                fromBuiltinData
                                ( \x5 ->
                                    done (f x1 x2 x3 x4 x5)
                                )
                          )
                    )
              )
        )

-- | @Since Unreleased
{-# ANN productFromData6 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData6 #-}
productFromData6 ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type).
  ( FromData a
  , FromData b
  , FromData c
  , FromData d
  , FromData e
  , FromData f
  ) =>
  (a -> b -> c -> d -> e -> f -> g) ->
  BuiltinData ->
  Maybe g
productFromData6 f =
  productFromData6'
    (\x1 x2 x3 x4 x5 x6 -> Just (f x1 x2 x3 x4 x5 x6))

-- | @Since Unreleased
{-# ANN productFromData6' ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productFromData6' #-}
productFromData6' ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type).
  ( FromData a
  , FromData b
  , FromData c
  , FromData d
  , FromData e
  , FromData f
  ) =>
  (a -> b -> c -> d -> e -> f -> Maybe g) ->
  BuiltinData ->
  Maybe g
productFromData6' f dat =
  Unsafe.chooseData dat Nothing Nothing (go (Unsafe.unsafeDataAsList dat)) Nothing Nothing
  where
    go :: Unsafe.BuiltinList BuiltinData -> Maybe g
    go =
      step
        fromBuiltinData
        ( \x1 ->
            step
              fromBuiltinData
              ( \x2 ->
                  step
                    fromBuiltinData
                    ( \x3 ->
                        step
                          fromBuiltinData
                          ( \x4 ->
                              step
                                fromBuiltinData
                                ( \x5 ->
                                    step
                                      fromBuiltinData
                                      ( \x6 ->
                                          done (f x1 x2 x3 x4 x5 x6)
                                      )
                                )
                          )
                    )
              )
        )

-- | @since Unreleased
{-# ANN productUnsafeFromData2 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productUnsafeFromData2 #-}
productUnsafeFromData2 ::
  forall (a :: Type) (b :: Type) (c :: Type).
  (UnsafeFromData a, UnsafeFromData b) =>
  (a -> b -> c) ->
  BuiltinData ->
  c
productUnsafeFromData2 f dat =
  step'
    unsafeFromBuiltinData
    ( \x ->
        done' unsafeFromBuiltinData (f x)
    )
    (Unsafe.unsafeDataAsList dat)

-- | @since Unreleased
{-# ANN productUnsafeFromData3 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productUnsafeFromData3 #-}
productUnsafeFromData3 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type).
  (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c) =>
  (a -> b -> c -> d) ->
  BuiltinData ->
  d
productUnsafeFromData3 f dat =
  step'
    unsafeFromBuiltinData
    ( \x ->
        step'
          unsafeFromBuiltinData
          ( \y ->
              done' unsafeFromBuiltinData (f x y)
          )
    )
    (Unsafe.unsafeDataAsList dat)

-- | @since Unreleased
{-# ANN productUnsafeFromData4 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productUnsafeFromData4 #-}
productUnsafeFromData4 ::
  forall (a :: Type) (b :: Type) (c :: Type) (d :: Type) (e :: Type).
  (UnsafeFromData a, UnsafeFromData b, UnsafeFromData c, UnsafeFromData d) =>
  (a -> b -> c -> d -> e) ->
  BuiltinData ->
  e
productUnsafeFromData4 f dat =
  step'
    unsafeFromBuiltinData
    ( \x1 ->
        step'
          unsafeFromBuiltinData
          ( \x2 ->
              step'
                unsafeFromBuiltinData
                ( \x3 ->
                    done' unsafeFromBuiltinData (f x1 x2 x3)
                )
          )
    )
    (Unsafe.unsafeDataAsList dat)

-- | @since Unreleased
{-# ANN productUnsafeFromData5 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productUnsafeFromData5 #-}
productUnsafeFromData5 ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type).
  ( UnsafeFromData a
  , UnsafeFromData b
  , UnsafeFromData c
  , UnsafeFromData d
  , UnsafeFromData e
  ) =>
  (a -> b -> c -> d -> e -> f) ->
  BuiltinData ->
  f
productUnsafeFromData5 f dat =
  step'
    unsafeFromBuiltinData
    ( \x1 ->
        step'
          unsafeFromBuiltinData
          ( \x2 ->
              step'
                unsafeFromBuiltinData
                ( \x3 ->
                    step'
                      unsafeFromBuiltinData
                      ( \x4 ->
                          done' unsafeFromBuiltinData (f x1 x2 x3 x4)
                      )
                )
          )
    )
    (Unsafe.unsafeDataAsList dat)

-- | @since Unreleased
{-# ANN productUnsafeFromData6 ("HLint: ignore Avoid lambda" :: HString.String) #-}
{-# INLINE productUnsafeFromData6 #-}
productUnsafeFromData6 ::
  forall
    (a :: Type)
    (b :: Type)
    (c :: Type)
    (d :: Type)
    (e :: Type)
    (f :: Type)
    (g :: Type).
  ( UnsafeFromData a
  , UnsafeFromData b
  , UnsafeFromData c
  , UnsafeFromData d
  , UnsafeFromData e
  , UnsafeFromData f
  ) =>
  (a -> b -> c -> d -> e -> f -> g) ->
  BuiltinData ->
  g
productUnsafeFromData6 f dat =
  step'
    unsafeFromBuiltinData
    ( \x1 ->
        step'
          unsafeFromBuiltinData
          ( \x2 ->
              step'
                unsafeFromBuiltinData
                ( \x3 ->
                    step'
                      unsafeFromBuiltinData
                      ( \x4 ->
                          step'
                            unsafeFromBuiltinData
                            ( \x5 ->
                                done' unsafeFromBuiltinData (f x1 x2 x3 x4 x5)
                            )
                      )
                )
          )
    )
    (Unsafe.unsafeDataAsList dat)

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

step1 ::
  forall (k :: Type).
  (k -> BuiltinData) ->
  k ->
  Unsafe.BuiltinList BuiltinData ->
  Unsafe.BuiltinList BuiltinData
step1 f x = Unsafe.mkCons (f x)

done ::
  forall (d :: Type).
  Maybe d ->
  Unsafe.BuiltinList BuiltinData ->
  Maybe d
done res ell = matchList ell res (\_ _ -> Nothing)

done' ::
  forall (k :: Type) (r :: Type).
  (BuiltinData -> k) ->
  (k -> r) ->
  Unsafe.BuiltinList BuiltinData ->
  r
done' f g ell = g (f (Unsafe.head ell))

done1 ::
  forall (k :: Type).
  (k -> BuiltinData) ->
  k ->
  Unsafe.BuiltinList BuiltinData
done1 f x = Unsafe.mkCons (f x) (Unsafe.mkNilData Unsafe.unitval)
