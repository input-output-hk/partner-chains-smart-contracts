{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Extra (
  suchThat,
  suchThatMap,
  suchThatRetrying,
  suchThatMapRetrying,
  sublistOf,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, modify)
import Data.Bits (testBit, unsafeShiftL)
import Data.Kind (Type)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen qualified as Gen
import Prelude

suchThat ::
  forall (a :: Type).
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThat = suchThatRetrying 100

suchThatMap ::
  forall (a :: Type) (b :: Type).
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMap = suchThatMapRetrying 100

suchThatRetrying ::
  forall (a :: Type).
  Word ->
  Gen a ->
  (a -> Bool) ->
  Gen a
suchThatRetrying limit gen p =
  suchThatHelper "suchThat" limit gen (\x -> if p x then Just x else Nothing)

suchThatMapRetrying ::
  forall (a :: Type) (b :: Type).
  Word ->
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatMapRetrying = suchThatHelper "suchThatMap"

sublistOf ::
  forall (a :: Type).
  [a] ->
  Gen [a]
sublistOf src = do
  let len = length src
  encoding <- Gen.chooseInteger (0, (1 `unsafeShiftL` len) - 1)
  pure . go encoding [] 0 $ src
  where
    go :: Integer -> [a] -> Int -> [a] -> [a]
    go encoding acc ix = \case
      [] -> reverse acc
      (x : xs) ->
        if testBit encoding ix
          then go encoding (x : acc) (ix + 1) xs
          else go encoding acc (ix + 1) xs

-- Helpers

suchThatHelper ::
  forall (a :: Type) (b :: Type).
  String ->
  Word ->
  Gen a ->
  (a -> Maybe b) ->
  Gen b
suchThatHelper label limit gen k = do
  res <- evalStateT go 0
  maybe (error $ label <> " exceeded retry limit: " <> show limit) pure res
  where
    go :: StateT Word Gen (Maybe b)
    go = do
      res <- k <$> lift gen
      case res of
        Nothing -> do
          count <- get
          if count == limit
            then pure Nothing
            else modify (+ 1) *> go
        Just _ -> pure res
