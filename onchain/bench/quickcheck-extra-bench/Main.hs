module Main (main) where

import Data.List (replicate)
import Data.String qualified as HaskellString
import GHC.Num qualified as GHCNum
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Extra (
  sublistOf,
  suchThat,
  suchThatMap,
 )
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen qualified as Gen
import Test.Tasty.Bench (
  Benchmark,
  bcompare,
  bench,
  bgroup,
  defaultMain,
  locateBenchmark,
  nfIO,
 )
import Test.Tasty.Patterns.Printer (printAwkExpr)
import TrustlessSidechain.HaskellPrelude

main :: IO ()
main =
  defaultMain
    [ bgroup
        "suchThat"
        [ bench "suchThat for even Integer (baseline)"
            . nfIO
            . benchGenerator1
            $ \(gen :: Gen Integer) -> Gen.suchThat gen even
        , bcompare (findBench "suchThat for even Integer (baseline)")
            . bench "suchThat for even Integer (new)"
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
        , bcompare (findBench "suchThatMap for even Integer (baseline)")
            . bench "suchThatMap for even Integer (new)"
            . nfIO
            . benchGenerator1
            $ \gen -> suchThatMap gen toNat
        ]
    , bgroup "sublistOf" . sublistOfBenches $ [10, 20 .. 100]
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
    let baselineName = "sublistOf (baseline, " <> show size <> " items)"
     in [ bench baselineName
            . nfIO
            . Gen.generate
            . Gen.sublistOf
            . make42s
            $ size
        , bcompare (findBench baselineName)
            . bench ("sublistOf (new, " <> show size <> " items)")
            . nfIO
            . Gen.generate
            . sublistOf
            . make42s
            $ size
        ]

-- Helpers

findBench :: HaskellString.String -> HaskellString.String
findBench = printAwkExpr . locateBenchmark . (: [])

toNat :: Integer -> Maybe Natural
toNat x
  | x < 0 = Nothing
  | otherwise = Just . GHCNum.fromInteger $ x

make42s :: Int -> [Integer]
make42s count = replicate count 42
