{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module is a bunch of tests for the key functions in the distributed set
module Test.TrustlessSidechain.DistributedSet (test) where

import Data.Char qualified as Char
import PlutusTx.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import TrustlessSidechain.OnChain.DistributedSet (DistributedSetDatum (DistributedSetDatum, dsBranches, dsLeaf), Node (Node, nodeDatum, nodeTokenName))
import TrustlessSidechain.OnChain.DistributedSet qualified as DS
import Prelude qualified

-- import Test.QuickCheck qualified as QuickCheck
-- import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, Testable)

test :: TestTree
test =
  Tasty.testGroup
    "DistributedSet"
    [ HUnit.testCase "Unit tests" Prelude.$ do
        let node =
              Node
                { nodeTokenName = ""
                , nodeDatum =
                    DistributedSetDatum
                      { dsLeaf = Just "a"
                      , dsBranches = []
                      }
                }

            nstr = "aa"

            ans =
              [ Node {nodeTokenName = "", nodeDatum = DistributedSetDatum {dsLeaf = Nothing, dsBranches = [Prelude.toInteger $ Char.ord 'a']}}
              , Node {nodeTokenName = "a", nodeDatum = DistributedSetDatum {dsLeaf = Just "", dsBranches = [Prelude.toInteger $ Char.ord 'a']}}
              , Node {nodeTokenName = "aa", nodeDatum = DistributedSetDatum {dsLeaf = Just "", dsBranches = []}}
              ]
        Just ans HUnit.@=? DS.nodeNexts nstr node
    ]
