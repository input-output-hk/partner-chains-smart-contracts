module TrustlessSidechain.Effects.Util
  ( fromEitherThrow
  , fromMaybeThrow
  , lmapThrow
  , mapError
  ) where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Run (Run)
import Run.Except (EXCEPT, note, rethrow, runExcept)
import Type.Row (type (+))

-- | Given an error mapping function and an Either, lift the error to the effect stack
lmapThrow :: forall e e' a r. (e -> e') -> Either e a -> Run (EXCEPT e' + r) a
lmapThrow f = rethrow <<< lmap f

-- | Given an effect stack with an EXCEPT effect, lift an either into the effect stack
fromEitherThrow ::
  forall e a r. Run (EXCEPT e + r) (Either e a) -> Run (EXCEPT e + r) a
fromEitherThrow ma = rethrow =<< ma

-- | Given an error and an effect stack with an EXCEPT effect, lift a maybe into the effect stack
fromMaybeThrow ::
  forall e a r. e -> Run (EXCEPT e + r) (Maybe a) -> Run (EXCEPT e + r) a
fromMaybeThrow e ma = note e =<< ma

-- | Flattens a stack of two exceptions, by mapping one into the other
mapError ::
  forall e' e r.
  (e' -> e) ->
  Run (EXCEPT e' + EXCEPT e + r) ~> Run (EXCEPT e + r)
mapError f = lmapThrow f <=< runExcept
