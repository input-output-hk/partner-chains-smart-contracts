-- | Utilities for handling `Maybe` data type.
module TrustlessSidechain.Utils.Maybe
  ( maybeToArray
  ) where

import Contract.Prelude

-- | Convert `Just` to a singleton array, and `Nothing` to an empty array.
maybeToArray ∷ ∀ a. Maybe a → Array a
maybeToArray (Just a) = [ a ]
maybeToArray Nothing = []
