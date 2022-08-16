{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | This module is a bunch of tests for the key functions in the distributed set.

 It essentially generates a mock utxo set (using 'Map.Map') and tests its
 equivalence to 'Data.Set.Set'.

 TODO: the tests are a bit messy.. they were originally written for a plain ol'
 ByteString (which works for 'Map.Map' that uses 'Prelude.Ord') and have been
 cobbled together to work for the BuiltinByteString type.
-}
module Test.TrustlessSidechain.DistributedSet (test) where

import Control.Monad qualified as Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Word (Word8)
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import PlutusTx.Prelude
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Gen, Positive (Positive), Property, QuickCheckMaxSize (QuickCheckMaxSize))
import Test.Tasty.QuickCheck qualified as QuickCheck
import TrustlessSidechain.OnChain.DistributedSet (Edge (Tip), Node (Node, nEdge, nPrefix))
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
    go n = maybe n (\npr -> go (Maybe.fromJust $ Map.lookup npr $ unDS ds)) $ nextNodePrefix str n

nextNodePrefix :: ByteString -> Node -> Maybe ByteString
nextNodePrefix str node = fmap unBuiltinByteString $ DS.nextNodePrefix (BuiltinByteString str) node

rootNode :: ByteString -> Node
rootNode str = Node {nPrefix = "", nEdge = Tip $ BuiltinByteString str}

initDS :: ByteString -> DS
initDS str = DS {unDS = Map.fromList [("", rootNode str)]}

insertDS :: ByteString -> DS -> DS
insertDS str ds =
  let node = lookupDS str ds
      nnodes = DS.toListIb $ DS.insertNode (BuiltinByteString str) node
   in DS $
        foldl
          (\acc n -> Map.alter (const (Just n)) (unBuiltinByteString $ nPrefix n) acc)
          (unDS ds)
          nnodes

toListDS :: DS -> [ByteString]
toListDS = foldMap (\n -> case nEdge n of Tip suf -> [unBuiltinByteString (nPrefix n) `B.append` unBuiltinByteString suf]; _ -> mempty) . Map.elems . unDS

prop_set :: Positive Int -> Property
prop_set (Positive n) = QuickCheck.property $
  QuickCheck.forAll (QuickCheck.arbitrary >>= \(Positive sz) -> Prelude.fmap Set.fromList $ (Monad.replicateM sz . Monad.replicateM n) (QuickCheck.arbitrary :: Gen Word8)) $
    \set ->
      let lst = map B.pack $ Set.toList set
       in QuickCheck.forAll (QuickCheck.shuffle lst) $ \lst' -> lst Prelude.== List.sort (toListDS (fromListDS lst'))

fromListDS :: [ByteString] -> DS
fromListDS (a : as) = foldl (flip insertDS) (initDS a) as
fromListDS [] = Prelude.error "error 'fromListDS' empty list"

test :: TestTree
test =
  Tasty.testGroup
    "DistributedSet"
    [ Tasty.testGroup
        "Properties"
        [ Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 256) $ QuickCheck.testProperty "Strings of size at most 256" $ QuickCheck.withMaxSuccess 100 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 64) $ QuickCheck.testProperty "Strings of size at most 64" $ QuickCheck.withMaxSuccess 100 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 8) $ QuickCheck.testProperty "Strings of size at most 8" $ QuickCheck.withMaxSuccess 100 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 4) $ QuickCheck.testProperty "Strings of size at most 4" $ QuickCheck.withMaxSuccess 100 prop_set
        , Tasty.adjustOption (\(QuickCheckMaxSize _) -> QuickCheckMaxSize 2) $ QuickCheck.testProperty "Strings of size at most 2" $ QuickCheck.withMaxSuccess 100 prop_set
        ]
    , Tasty.testGroup
        "Unit tests"
        [ HUnit.testCase "Prefixes" $ do
            let tst = ["ab", "aa"] in List.sort tst HUnit.@=? toListDS (fromListDS tst)
            let tst = ["aa", "ab"] in List.sort tst HUnit.@=? toListDS (fromListDS tst)
            let tst = ["ba", "ab"] in List.sort tst HUnit.@=? toListDS (fromListDS tst)
            let tst = ["ab", "ba"] in List.sort tst HUnit.@=? toListDS (fromListDS tst)
        ]
    ]
