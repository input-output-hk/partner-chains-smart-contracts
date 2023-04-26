{-# LANGUAGE TypeApplications #-}

{- | This module is a bunch of tests for our MerkleTree implementation. For
 now, it's just property based tests.
-}
module Test.TrustlessSidechain.MerkleTree (test) where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import PlutusPrelude (NonEmpty ((:|)))
import PlutusTx.Builtins.Class qualified as Builtins
import PlutusTx.Prelude
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, Testable)
import Test.QuickCheck qualified as QuickCheck
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck (testProperty)
import TrustlessSidechain.MerkleTree qualified as MT
import Prelude qualified

test :: TestTree
test =
  Tasty.testGroup
    "MerkleTree"
    [ testProperty "height is logarithmic to size" logProofLength
    , testProperty "proof in source implies proof in tree" lookupMpHasProof
    , testProperty "proof in tree implies proof in source" notInLookupMpFail
    , testProperty "proof in tree implies proof in root" inListMemberMp
    , testProperty "lookups as expected" lookupsMp1
    , testProperty "lookups as expected (converse)" lookupsMp1Converse
    , testProperty "lookups as expected (extra)" lookupsMp2
    ]

-- Properties

{- | Property.
    Let lst be an arbitrary non empty list.
        height (fromNonEmpty lst) <= floor(log_2 (length lst)) + 2
-}
logProofLength :: Property
logProofLength = forAllNonEmptyBuiltinByteString $
  \lst@(a :| as) ->
    let lst' = a : as
        tree = MT.fromNonEmpty lst
     in MT.height tree <= Prelude.floor (Prelude.logBase @Prelude.Double 2 (Prelude.fromIntegral (length lst'))) + 2

{- | Property.
  x \in lst ==> isJust (MT.lookupMp x (MT.fromNonEmpty lst))
-}
lookupMpHasProof :: Property
lookupMpHasProof =
  forAllNonEmptyBuiltinByteStringWithElem $
    \(lst@(_a :| _as), x) -> isJust $ MT.lookupMp x $ MT.fromNonEmpty lst

{- | Property.
  x \in lst <== isJust (MT.lookupMp x (MT.fromNonEmpty lst))

 but we test this via the contrapositive.
-}
notInLookupMpFail :: Property
notInLookupMpFail =
  forAllNonEmptyBuiltinByteStringWithoutElem $
    \(lst@(_ :| _), x) ->
      let tree = MT.fromNonEmpty lst
       in isNothing (MT.lookupMp x tree)

{- | Property.
 Suppose lst is an arbitrary non empty list.
  Let tree = fromNonEmpty lst

  Just prf = lookupMp x tree ==> memberMp x prf (rootHash tree) = True

 TODO: Didn't test the converse -- it's a bit trickier to test, and the large search space
 makes me doubt the usefulness of QuickCheck for this.
-}
inListMemberMp :: Property
inListMemberMp =
  forAllNonEmptyBuiltinByteStringWithElem $ \(lst@(_a :| _as), x) ->
    let tree = MT.fromNonEmpty lst
     in QuickCheck.property $ case MT.lookupMp x tree of
          Nothing -> False
          Just prf -> MT.memberMp x prf . MT.rootHash $ tree

{-
 Properties.
    1. Suppose lst is an arbitrary non empty list of distinct elements.
            (roothash, merkleProof) \in lookupsMp (fromNonEmpty lst)
                ===> there exists x \in lst s.t.
                    Just merkleProof' (lookupMp x (fromNonEmpty lst)),
                    merkleProof == merkleProof',
                    rootHash = hashLeaf x

            x \in lst,  Just merkleProof = (lookupMp x (fromNonEmpty lst))
                ===> (hashLeaf x, merkleProof) \in  lookupsMp (fromNonEmpty lst)
    2. Suppose lst is an arbitrary non empty list of length n.
        length (lookupsMp (fromNonEmpty lst)) == n
-}
lookupsMp1 :: Property
lookupsMp1 =
  forAllNonEmptyDistinctBuiltinByteString $ \(a :| as) ->
    let mt = MT.fromList (a : as)
     in QuickCheck.forAll (QuickCheck.elements (MT.lookupsMp mt)) $
          \(rh, mp) ->
            Prelude.any
              ( \x ->
                  MT.hashLeaf x == rh
                    && Maybe.fromJust (MT.lookupMp x mt) Prelude.== mp
              )
              $ a : as

lookupsMp1Converse :: Property
lookupsMp1Converse =
  forAllNonEmptyDistinctBuiltinByteString $ \(a :| as) ->
    let mt = MT.fromList (a : as)
        prfs = MT.lookupsMp mt
     in QuickCheck.forAll (QuickCheck.elements (a : as)) $ \x ->
          let mp = Maybe.fromJust $ MT.lookupMp x mt
           in Maybe.fromJust (List.lookup (MT.hashLeaf x) prfs) Prelude.== mp

lookupsMp2 :: Property
lookupsMp2 =
  forAllNonEmptyBuiltinByteString $ \(a :| as) ->
    let mt = MT.fromList (a : as)
     in length (MT.lookupsMp mt) == length (a : as)

-- Helpers

{- | 'genNonEmptyBuiltinByteString' randomly generates 'NonEmpty' lists of
 'BuiltinByteString'
-}
genNonEmptyBuiltinByteString :: Gen (NonEmpty BuiltinByteString)
genNonEmptyBuiltinByteString = do
  h <- arbitrary
  t <- arbitrary
  Prelude.pure . Prelude.fmap Builtins.stringToBuiltinByteString $ h :| t

{- | @forAllNonEmptyBuiltinByteString prf@ is read as "for every nonempty list
 of BuiltinByteString, @prf@ is satisified.."
-}
forAllNonEmptyBuiltinByteString ::
  Testable prop =>
  (NonEmpty BuiltinByteString -> prop) ->
  Property
forAllNonEmptyBuiltinByteString = QuickCheck.forAll genNonEmptyBuiltinByteString

{- | @forAllNonEmptyDistinctBuiltinByteString prf@ is read as "for every nonempty list
 of distinct BuiltinByteStrings, @prf@ is satisified.."
-}
forAllNonEmptyDistinctBuiltinByteString ::
  Testable prop =>
  (NonEmpty BuiltinByteString -> prop) ->
  Property
forAllNonEmptyDistinctBuiltinByteString =
  QuickCheck.forAll
    (Prelude.fmap (NonEmpty.fromList . List.nub . NonEmpty.toList) genNonEmptyBuiltinByteString)

{- | @forAllNonEmptyBuiltinByteStringWithElem prf@ is read as "for every nonempty list @lst@, and @x \in lst@
 of BuiltinByteString, @prf (lst, x)@ is satisified.."
-}
forAllNonEmptyBuiltinByteStringWithElem ::
  Testable prop =>
  ((NonEmpty BuiltinByteString, BuiltinByteString) -> prop) ->
  Property
forAllNonEmptyBuiltinByteStringWithElem = QuickCheck.forAll genNonEmptyWithElem
  where
    genNonEmptyWithElem :: Gen (NonEmpty BuiltinByteString, BuiltinByteString)
    genNonEmptyWithElem = do
      a :| as <- genNonEmptyBuiltinByteString
      x <- QuickCheck.elements $ a : as
      return (a :| as, x)

{- | @forAllNonEmptyBuiltinByteStringWithoutElem prf@ is read as "for every nonempty list @lst@, and @x \not \in lst@
 of BuiltinByteString, @prf (lst, x)@ is satisified.."
-}
forAllNonEmptyBuiltinByteStringWithoutElem ::
  Testable prop =>
  ((NonEmpty BuiltinByteString, BuiltinByteString) -> prop) ->
  Property
forAllNonEmptyBuiltinByteStringWithoutElem = QuickCheck.forAll genNonEmptyWithoutElem
  where
    genNonEmptyWithoutElem :: Gen (NonEmpty BuiltinByteString, BuiltinByteString)
    genNonEmptyWithoutElem = do
      a :| as <- genNonEmptyBuiltinByteString
      x <- Prelude.fmap Builtins.stringToBuiltinByteString (QuickCheck.arbitrary :: Gen Prelude.String)
      if x `elem` (a : as)
        then QuickCheck.discard
        else return (a :| as, x)
