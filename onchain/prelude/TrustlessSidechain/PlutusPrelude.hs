module TrustlessSidechain.PlutusPrelude (
  module PlutusTx.Prelude,
  GHC.IsString (..),
  HaskellPrelude.fromInteger,
  ifThenElse,
) where

import Data.Kind (Type)
import GHC.Exts qualified as GHC
import PlutusTx.Prelude hiding (fromInteger, toList)
import Prelude qualified as HaskellPrelude

{-# INLINE ifThenElse #-}
ifThenElse :: forall (a :: Type). Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
