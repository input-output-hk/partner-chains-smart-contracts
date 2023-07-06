module TrustlessSidechain.PlutusPrelude (
  module PlutusTx.Prelude,
  GHC.IsString (..),
  PlutusTx.ToData (..),
  PlutusTx.FromData (..),
  PlutusTx.UnsafeFromData (..),
  HaskellPrelude.fromInteger,
  ifThenElse,

  -- * Helpers for Data encoding
  productToData2,
  productFromData2,
  productUnsafeFromData2,
) where

import Data.Kind (Type)
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
productToData2 x y =
  Unsafe.mkList
    ( Unsafe.mkCons
        (toBuiltinData x)
        ( Unsafe.mkCons
            (toBuiltinData y)
            (Unsafe.mkNilData Unsafe.unitval)
        )
    )

-- | @since Unreleased
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

-- | @since Unreleased
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
