{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | This module is a bunch of tests for our MerkleTree implementation. For
 now, it's just property based tests.
-}
module Test.TrustlessSidechain.MerkleTree (test) where

import PlutusPrelude (NonEmpty ((:|)))
import PlutusTx.Builtins.Class qualified as Builtins
import PlutusTx.Prelude
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property, Testable)
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (getNonEmpty))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck qualified as QuickCheck
import TrustlessSidechain.MerkleTree qualified as MT
import Prelude qualified

{- | 'genNonEmptyBuiltinByteString' randomly generates 'NonEmpty' lists of
 'BuiltinByteString'
-}
genNonEmptyBuiltinByteString :: Gen (NonEmpty BuiltinByteString)
genNonEmptyBuiltinByteString = do
  lst <- Prelude.fmap getNonEmpty (arbitrary :: Gen (NonEmptyList Prelude.String))
  let a : as = lst
  return $ Prelude.fmap Builtins.stringToBuiltinByteString $ a :| as

{- | @forAllNonEmptyBuiltinByteString prf@ is read as "for every nonempty list
 of BuiltinByteString, @prf@ is satisified.."
-}
forAllNonEmptyBuiltinByteString :: Testable prop => (NonEmpty BuiltinByteString -> prop) -> Property
forAllNonEmptyBuiltinByteString = QuickCheck.forAll genNonEmptyBuiltinByteString

{- | @forAllNonEmptyBuiltinByteStringWithElem prf@ is read as "for every nonempty list @lst@, and @x \in lst@
 of BuiltinByteString, @prf (lst, x)@ is satisified.."
-}
forAllNonEmptyBuiltinByteStringWithElem :: Testable prop => ((NonEmpty BuiltinByteString, BuiltinByteString) -> prop) -> Property
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
forAllNonEmptyBuiltinByteStringWithoutElem :: Testable prop => ((NonEmpty BuiltinByteString, BuiltinByteString) -> prop) -> Property
forAllNonEmptyBuiltinByteStringWithoutElem = QuickCheck.forAll genNonEmptyWithoutElem
  where
    genNonEmptyWithoutElem :: Gen (NonEmpty BuiltinByteString, BuiltinByteString)
    genNonEmptyWithoutElem = do
      a :| as <- genNonEmptyBuiltinByteString
      x <- Prelude.fmap Builtins.stringToBuiltinByteString (QuickCheck.arbitrary :: Gen Prelude.String)
      if x `elem` (a : as)
        then QuickCheck.discard
        else return (a :| as, x)

{- | Property.
    Let lst be an arbitrary non empty list.
        height (fromNonEmpty lst) <= floor(log_2 (length lst)) + 2
-}
prop_logProofLength :: Property
prop_logProofLength = forAllNonEmptyBuiltinByteString $
  \lst@(a :| as) ->
    let lst' = a : as
        tree = MT.fromNonEmpty lst
     in MT.height tree <= Prelude.floor (Prelude.logBase @Prelude.Double 2 (Prelude.fromIntegral (length lst'))) + 2

{- | Property.
  x \in lst ==> isJust (MT.lookupMp x (MT.fromNonEmpty lst))
-}
prop_lookupMpHasProof :: Property
prop_lookupMpHasProof =
  forAllNonEmptyBuiltinByteStringWithElem $
    \(lst@(_a :| _as), x) -> isJust $ MT.lookupMp x $ MT.fromNonEmpty lst

{- | Property.
  x \in lst <== isJust (MT.lookupMp x (MT.fromNonEmpty lst))

 but we test this via the contrapositive.
-}
prop_notInLookupMpFail :: Property
prop_notInLookupMpFail =
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
prop_inListMemberMp :: Property
prop_inListMemberMp =
  forAllNonEmptyBuiltinByteStringWithElem $ \(lst@(_a :| _as), x) ->
    let tree = MT.fromNonEmpty lst
        Just prf = MT.lookupMp x tree
     in MT.memberMp x prf (MT.rootHash tree)

-- This is needed because of QuickCheck. It's explained in the QuickCheck
-- documentation.
return []

test :: TestTree
test =
  Tasty.testGroup
    "MerkleTree"
    [ QuickCheck.testProperties "Properties" $(QuickCheck.allProperties)
    ]
