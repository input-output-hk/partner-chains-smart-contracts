module Main (main) where

import Test.Tasty
import Test.TrustlessSidechain.OnChain.CommitteeCandidateValidator as CommitteeCandidateValidator
import Prelude (IO)

-- | @since 0.1
main :: IO ()
main = defaultMain tests

{- | Project wide tests

 @since 0.1
-}
tests :: TestTree
tests =
  testGroup
    "TrustlessSidechain"
    [CommitteeCandidateValidator.test]
