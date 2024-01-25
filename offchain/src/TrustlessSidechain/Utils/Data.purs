-- | Provides mirrors of the equivalent onchain functions as provided by
-- | onchain's Plutus-specific prelude. These are designed to ensure consistent
-- | Data serializations in both places.
-- |
-- | Unlike onchain, there is no equivalent to UnsafeFromData: thus, we do not
-- | provide mirrors of those functions.
module TrustlessSidechain.Utils.Data
  ( productToData2
  , productToData3
  , productToData4
  , productToData5
  , productToData6
  , productFromData2
  , productFromData2'
  , productFromData3
  , productFromData3'
  , productFromData4
  , productFromData4'
  , productFromData5
  , productFromData5'
  , productFromData6
  , productFromData6'
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(List)
  , fromData
  , toData
  )

productToData2 ∷
  ∀ (a ∷ Type) (b ∷ Type).
  ToData a ⇒
  ToData b ⇒
  a →
  b →
  PlutusData
productToData2 x1 x2 = List [ toData x1, toData x2 ]

productToData3 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type).
  ToData a ⇒
  ToData b ⇒
  ToData c ⇒
  a →
  b →
  c →
  PlutusData
productToData3 x1 x2 x3 = List [ toData x1, toData x2, toData x3 ]

productToData4 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type).
  ToData a ⇒
  ToData b ⇒
  ToData c ⇒
  ToData d ⇒
  a →
  b →
  c →
  d →
  PlutusData
productToData4 x1 x2 x3 x4 =
  List [ toData x1, toData x2, toData x3, toData x4 ]

productToData5 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type).
  ToData a ⇒
  ToData b ⇒
  ToData c ⇒
  ToData d ⇒
  ToData e ⇒
  a →
  b →
  c →
  d →
  e →
  PlutusData
productToData5 x1 x2 x3 x4 x5 =
  List [ toData x1, toData x2, toData x3, toData x4, toData x5 ]

productToData6 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type) (f ∷ Type).
  ToData a ⇒
  ToData b ⇒
  ToData c ⇒
  ToData d ⇒
  ToData e ⇒
  ToData f ⇒
  a →
  b →
  c →
  d →
  e →
  f →
  PlutusData
productToData6 x1 x2 x3 x4 x5 x6 =
  List [ toData x1, toData x2, toData x3, toData x4, toData x5, toData x6 ]

productFromData2 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type).
  FromData a ⇒
  FromData b ⇒
  (a → b → c) →
  PlutusData →
  Maybe c
productFromData2 f = productFromData2' (\x1 x2 → Just (f x1 x2))

productFromData2' ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type).
  FromData a ⇒
  FromData b ⇒
  (a → b → Maybe c) →
  PlutusData →
  Maybe c
productFromData2' f = case _ of
  List [ x1, x2 ] → do
    x1' ← fromData x1
    x2' ← fromData x2
    f x1' x2'
  _ → Nothing

productFromData3 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  (a → b → c → d) →
  PlutusData →
  Maybe d
productFromData3 f = productFromData3' (\x1 x2 x3 → Just (f x1 x2 x3))

productFromData3' ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  (a → b → c → Maybe d) →
  PlutusData →
  Maybe d
productFromData3' f = case _ of
  List [ x1, x2, x3 ] → do
    x1' ← fromData x1
    x2' ← fromData x2
    x3' ← fromData x3
    f x1' x2' x3'
  _ → Nothing

productFromData4 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  FromData d ⇒
  (a → b → c → d → e) →
  PlutusData →
  Maybe e
productFromData4 f = productFromData4' (\x1 x2 x3 x4 → Just (f x1 x2 x3 x4))

productFromData4' ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  FromData d ⇒
  (a → b → c → d → Maybe e) →
  PlutusData →
  Maybe e
productFromData4' f = case _ of
  List [ x1, x2, x3, x4 ] → do
    x1' ← fromData x1
    x2' ← fromData x2
    x3' ← fromData x3
    x4' ← fromData x4
    f x1' x2' x3' x4'
  _ → Nothing

productFromData5 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type) (f ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  FromData d ⇒
  FromData e ⇒
  (a → b → c → d → e → f) →
  PlutusData →
  Maybe f
productFromData5 f = productFromData5'
  ( \x1 x2 x3 x4 x5 → Just
      (f x1 x2 x3 x4 x5)
  )

productFromData5' ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type) (f ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  FromData d ⇒
  FromData e ⇒
  (a → b → c → d → e → Maybe f) →
  PlutusData →
  Maybe f
productFromData5' f = case _ of
  List [ x1, x2, x3, x4, x5 ] → do
    x1' ← fromData x1
    x2' ← fromData x2
    x3' ← fromData x3
    x4' ← fromData x4
    x5' ← fromData x5
    f x1' x2' x3' x4' x5'
  _ → Nothing

productFromData6 ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type) (f ∷ Type) (g ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  FromData d ⇒
  FromData e ⇒
  FromData f ⇒
  (a → b → c → d → e → f → g) →
  PlutusData →
  Maybe g
productFromData6 f =
  productFromData6' (\x1 x2 x3 x4 x5 x6 → Just (f x1 x2 x3 x4 x5 x6))

productFromData6' ∷
  ∀ (a ∷ Type) (b ∷ Type) (c ∷ Type) (d ∷ Type)
    (e ∷ Type) (f ∷ Type) (g ∷ Type).
  FromData a ⇒
  FromData b ⇒
  FromData c ⇒
  FromData d ⇒
  FromData e ⇒
  FromData f ⇒
  (a → b → c → d → e → f → Maybe g) →
  PlutusData →
  Maybe g
productFromData6' f = case _ of
  List [ x1, x2, x3, x4, x5, x6 ] → do
    x1' ← fromData x1
    x2' ← fromData x2
    x3' ← fromData x3
    x4' ← fromData x4
    x5' ← fromData x5
    x6' ← fromData x6
    f x1' x2' x3' x4' x5' x6'
  _ → Nothing
