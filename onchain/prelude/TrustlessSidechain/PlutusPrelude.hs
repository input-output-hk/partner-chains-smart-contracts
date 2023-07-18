{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module TrustlessSidechain.PlutusPrelude (
  module PlutusTx.Prelude,
  GHC.IsString (..),
  HaskellPrelude.fromInteger,
  ifThenElse,
  HasField (..),
  put,
) where

import Data.Kind (Type)
import GHC.Exts qualified as GHC
import GHC.TypeLits (Symbol)
import PlutusTx.Prelude hiding (fromInteger, toList)
import Prelude qualified as HaskellPrelude

{-# INLINE ifThenElse #-}
ifThenElse :: forall (a :: Type). Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

{- | Describes that a structure @r@ has a field labelled @l@ with the type @a@.

 = Laws

 For each of these laws, we assume the same label is used throughout; given
 that the laws don't specify anything about the label (except that it's
 consistent throughout), we omit it.

 1. @'modify' 'id'@ @=@ @'id'@
 2. @'modify' f '.' 'modify' g@ @=@ @'modify' (f '.' g)@
 3. @'get' '.' 'modify' f@ @=@ @f '.' 'get'@

 @since Unreleased
-}
class HasField (l :: Symbol) (r :: Type) (a :: Type) | l r -> a where
  get :: r -> a
  modify :: (a -> a) -> r -> r

{- | @'put' x@ is a shorter form of @'modify' ('const' x)@.

 @since Unreleased
-}
put ::
  forall (l :: Symbol) (r :: Type) (a :: Type).
  HasField l r a =>
  a ->
  r ->
  r
put x = modify @l (const x)
