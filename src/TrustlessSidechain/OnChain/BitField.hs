{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}

{- | Implementation of onchain bitfield operations.
 The author really hopes that
 [this](https://github.com/cardano-foundation/CIPs/pull/283) becomes a thing
 which would COMPLETELY deprecate and remove this module from existence :) and
 make our lives much better. Go Koz Ross! I believe in you :)
-}
module TrustlessSidechain.OnChain.BitField where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Plutus.V1.Ledger.Orphans ()
import PlutusPrelude qualified
import PlutusTx (makeIsDataIndexed)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import Prelude qualified

{- | 'Byte' is an internal type alias to denote an element of a ByteString.
 Invariant: 'Byte' are values in [0, 2^8 - 1].
-}
type Byte = Integer

{- | newtype wrapper around a 'BuiltinByteString' as a bitfield.

 Short comings of this representation:

      * It doesn't store the length so there's no way of knowing how many
      bits this contains. But this is fine in the use case of the DistributedSet.
-}
newtype BitField = BitField {unBitField :: BuiltinByteString}
  deriving stock (Prelude.Show, Prelude.Eq, PlutusPrelude.Generic)

instance Eq BitField where
  (==) = (==) `PlutusPrelude.on` unBitField

makeIsDataIndexed ''BitField [('BitField, 0)]
deriveJSON defaultOptions ''BitField

{- | @'testByte' b ix@ returns 'True' iff the @ix@th bit of @b@ is @1@ (and
 returns 0 otherwise).

 In the case that @ix@ is negative, this throws an exception.
-}
{-# INLINEABLE testByte #-}
testByte :: Byte -> Integer -> Bool
testByte byte ix
  | ix < 0 = traceError "error 'testByte': negative index."
  | otherwise = go byte ix
  where
    go b i
      | i == 0 = (b `Builtins.remainderInteger` 2) == 1
      | otherwise = go (b `Builtins.quotientInteger` 2) (i - 1)

-- | @'orByte' a b@ returns the bitwise or of @a@ and @b@.
{-# INLINEABLE orByte #-}
orByte :: Byte -> Byte -> Byte
orByte = go 0 0
  where
    go acc st l r
      | st <= 7 =
        if l `Builtins.remainderInteger` 2 == 1 || r `Builtins.remainderInteger` 2 == 1
          then go (acc + exps0To7 st) (st + 1) (l `Builtins.quotientInteger` 2) (r `Builtins.quotientInteger` 2)
          else go acc (st + 1) (l `Builtins.quotientInteger` 2) (r `Builtins.quotientInteger` 2)
      | otherwise = acc

{- | @'exps0To7' i@ is an internal function to which does computes @2^i@ for @0
 <= i < 8@.
-}
{-# INLINEABLE exps0To7 #-}
exps0To7 :: Integer -> Integer
-- For some reason, string literals don't seem to compile with Plutus, so we
-- can't do a lookup table as the following implementation:
-- > exps0To7 i = i `Builtins.indexByteString` "\001\002\004\008\016\032\064\128"
--
-- For now, we do a binary search which implements the mapping
-- > exps0To7 i
-- >    | i == 0 = 1
-- >    | i == 1 = 2
-- >    | i == 2 = 4
-- >    | i == 3 = 8
-- >    | i == 4 = 16
-- >    | i == 5 = 32
-- >    | i == 6 = 64
-- >    | i == 7 = 128
-- >    | otherwise = traceError "error 'exps0To7' non-exhaustive."
exps0To7 i
  | i < 4 = if i < 2 then if i < 1 then 1 else 2 else if i < 3 then 4 else 8
  | i < 6 = if i < 5 then 16 else 32
  | i < 7 = 64
  | otherwise = 128

-- | @'setByte' b i@ sets the @i@th bit of @b@.
{-# INLINEABLE setByte #-}
setByte :: Byte -> Integer -> Byte
setByte b ix = orByte b $ exps0To7 ix

{- | @'testBitField' bf i@ returns 'True' iff the @i@th bit is 1 (and 'False'
 otherwise). This throws an exception in the case that @i@ is negative
-}
{-# INLINEABLE testBitField #-}
testBitField :: BitField -> Integer -> Bool
testBitField bit i
  | i < 0 = traceError "error 'testByte': negative index."
  | otherwise = testByte (indexByteString (unBitField bit) (i `Builtins.quotientInteger` 8)) (i `Builtins.remainderInteger` 8)

-- | @'setBitField' bf i@ sets the @i@th bit of @bf@
{-# INLINEABLE setBitField #-}
setBitField :: BitField -> Integer -> BitField
setBitField bf i =
  BitField $
    ( \bs ->
        takeByteString i' bs
          `appendByteString` Builtins.consByteString
            (setByte (Builtins.indexByteString bs i') (i `Builtins.remainderInteger` 8))
            (dropByteString (i' + 1) bs)
    )
      $ unBitField bf
  where
    i' = i `Builtins.quotientInteger` 8

{- | 'zeroes256' is 256 bits of zeroes i.e., this is @256 bits * (1 bytes /
 8bits) = 32 bytes@.
-}
{-# INLINEABLE zeroes256 #-}
zeroes256 :: BitField
-- Unfortunately, string literals don't compile for some reason, so we can't do something like this:
-- > zeroes256 = BitField "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
zeroes256 = BitField (dbl (dbl (dbl (dbl (dbl (Builtins.consByteString 0 emptyByteString))))))
  where
    -- Clever divide and conquer alg. from Hazem :)
    dbl :: BuiltinByteString -> BuiltinByteString
    dbl z = z `Builtins.appendByteString` z
