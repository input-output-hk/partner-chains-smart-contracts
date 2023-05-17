{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Kind (Type)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Extra (sublistOf, suchThat, suchThatMap)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen qualified as Gen
import Test.Tasty.Bench (
  Benchmark,
  bench,
  bgroup,
  defaultMain,
  nfIO,
 )
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup
        "suchThat"
        [ bench "suchThat for even Integer (baseline)"
            . nfIO
            . benchGenerator1
            $ \(gen :: Gen Integer) -> Gen.suchThat gen even
        , bench "suchThat for even Integer (new)"
            . nfIO
            . benchGenerator1
            $ \(gen :: Gen Integer) -> suchThat gen even
        ]
    , bgroup
        "suchThatMap"
        [ bench "suchThatMap for even Integer (baseline)"
            . nfIO
            . benchGenerator1
            $ \gen -> Gen.suchThatMap gen toNat
        , bench "suchThatMap for even Integer (new)"
            . nfIO
            . benchGenerator1
            $ \gen -> suchThatMap gen toNat
        ]
    , bgroup "sublistOf" . sublistOfBenches . powersOf2 $ 10
    ]

-- Benching functions

benchGenerator1 ::
  forall (a :: Type) (b :: Type).
  (Arbitrary a) =>
  (Gen a -> Gen b) ->
  IO b
benchGenerator1 lifter = Gen.generate . lifter $ arbitrary

sublistOfBenches :: [Int] -> [Benchmark]
sublistOfBenches sizes =
  sizes >>= \size ->
    [ bench ("sublistOf (baseline, " <> show size <> " items)")
        . nfIO
        . Gen.generate
        . Gen.sublistOf
        . make42s
        $ size
    , bench ("sublistOf (new, " <> show size <> " items)")
        . nfIO
        . Gen.generate
        . sublistOf
        . make42s
        $ size
    ]

-- Helpers

toNat :: Integer -> Maybe Natural
toNat x
  | x < 0 = Nothing
  | otherwise = Just . fromIntegral $ x

make42s :: Int -> [Integer]
make42s count = replicate count 42

powersOf2 :: Int -> [Int]
powersOf2 = go 1
  where
    go :: Int -> Int -> [Int]
    go curr remaining
      | remaining == 0 = []
      | otherwise = curr : go (curr * 2) (remaining - 1)
