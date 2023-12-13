module Main (main) where

import Data.List (isSubsequenceOf)
import Test.QuickCheck (
  Property,
  arbitrary,
  counterexample,
  forAllShrinkShow,
  generate,
  shrink,
 )
import Test.QuickCheck.Extra (sublistOf)
import Test.QuickCheck.Monadic (assert, monadicIO, monitor, run)
import Test.QuickCheck.Poly (A)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import TrustlessSidechain.HaskellPrelude

main :: IO ()
main =
  defaultMain
    . adjustOption go
    . testGroup "sublistOf"
    $ [ testProperty "generates only subsequences" propSubsequences
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000

-- Properties

propSubsequences :: Property
propSubsequences = forAllShrinkShow arbitrary shrink show $ \xs ->
  monadicIO $ do
    subsequence :: [A] <- run . generate . sublistOf $ xs
    monitor . counterexample $ "Generated: " <> show subsequence
    assert . isSubsequenceOf subsequence $ xs
