{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | This module is a bunch of tests for the key functions in the distributed set

 TODO: the tests are a bit messy.. they were originally written for a plain
 ol' ByteString and have been cobbled together to work for the
 BuiltinByteString type.
-}
module Test.TrustlessSidechain.DistributedSet (test) where

import Control.Monad qualified as Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word8)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck (Property, QuickCheckMaxSize (QuickCheckMaxSize))
import Test.Tasty.QuickCheck qualified as QuickCheck
import TrustlessSidechain.OnChain.DistributedSet (Node (Node, nBranches, nLeaf, nPrefix))
import TrustlessSidechain.OnChain.DistributedSet qualified as DS
import Prelude (Int)
import Prelude qualified

unBuiltinByteString :: BuiltinByteString -> ByteString
unBuiltinByteString (BuiltinByteString str) = str

newtype DS = DS {unDS :: Map ByteString Node}

lookupDS :: ByteString -> DS -> Node
lookupDS str ds =
  let root = Maybe.fromJust $ Map.lookup "" $ unDS ds
   in go root
  where
    go :: Node -> Node
    go n = maybe n (\nLength -> go (Maybe.fromJust $ Map.lookup (B.take nLength str) $ unDS ds)) $ nextNodeStrLength str n

nextNodeStrLength :: ByteString -> Node -> Maybe Int
nextNodeStrLength str node
  | unBuiltinByteString (nPrefix node) Prelude.== str = Nothing
  | unBuiltinByteString (nPrefix node) `B.isPrefixOf` str =
    nBranches node >>= \(branchPrefix, _ds) ->
      B.length (unBuiltinByteString $ nPrefix node) Prelude.+ B.length (unBuiltinByteString branchPrefix) Prelude.+ 1 <$ Monad.guard (DS.existsNextNode (BuiltinByteString str) node)
  -- N.B. this can be optimized a bit -- there's no need to contiuously check the
  | otherwise = Nothing

rootNode :: Node
rootNode = Node {nPrefix = "", nLeaf = False, nBranches = Nothing}

initDS :: DS
initDS = DS {unDS = Map.fromList [("", rootNode)]}

insertDS :: ByteString -> DS -> DS
insertDS str ds =
  let node = lookupDS str ds
      nnodes = DS.insertNode (BuiltinByteString str) node
   in DS $ foldl (\acc n -> Map.alter (const (Just n)) (unBuiltinByteString $ nPrefix n) acc) (unDS ds) $ Maybe.fromJust nnodes

toListDS :: DS -> [ByteString]
toListDS = foldMap (\n -> if nLeaf n then [unBuiltinByteString $ nPrefix n] else mempty) . Map.elems . unDS

fromListDS :: [ByteString] -> DS
fromListDS = foldl (flip insertDS) initDS

prop_set :: Set [Word8] -> Property
prop_set st = QuickCheck.property $ QuickCheck.forAll (QuickCheck.shuffle lst) $ \lst' -> lst Prelude.== List.sort (toListDS (fromListDS lst'))
  where
    lst = map B.pack $ Set.toList st

test :: TestTree
test =
  Tasty.testGroup
    "DistributedSet"
    [ Tasty.testGroup
        "Properties."
        [ Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 256) $ QuickCheck.testProperty "Strings of size at most 256" $ QuickCheck.withMaxSuccess 1000 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 64) $ QuickCheck.testProperty "Strings of size at most 64" $ QuickCheck.withMaxSuccess 1000 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 8) $ QuickCheck.testProperty "Strings of size at most 8" $ QuickCheck.withMaxSuccess 1000 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 4) $ QuickCheck.testProperty "Strings of size at most 4" $ QuickCheck.withMaxSuccess 1000 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 2) $ QuickCheck.testProperty "Strings of size at most 2" $ QuickCheck.withMaxSuccess 1000 prop_set
        ]
    ]
