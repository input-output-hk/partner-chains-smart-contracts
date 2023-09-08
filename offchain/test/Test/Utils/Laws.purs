module Test.Utils.Laws
  ( toDataLaws
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , fromData
  , toData
  )
import Test.QuickCheck
  ( assertEquals
  , quickCheckGen'
  , withHelp
  )
import Test.QuickCheck.Gen (Gen)

-- | Helper to verify that 'toData >>> fromData' is 'Just' for the given type.
toDataLaws ∷
  ∀ (a ∷ Type).
  ToData a ⇒
  FromData a ⇒
  Eq a ⇒
  Show a ⇒
  Int →
  Gen a →
  Effect Unit
toDataLaws tests gen = quickCheckGen' tests $ do
  x ← gen
  let y = fromData $ toData x
  pure $ case y of
    Nothing → withHelp false "Could not deserialize"
    Just y' → assertEquals x y'
