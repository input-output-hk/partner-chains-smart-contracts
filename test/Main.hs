module Main (main) where

import Test.Tasty
import Test.TrustlessSidechain.OnChain.Integration as Integration
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
    [Integration.test]
