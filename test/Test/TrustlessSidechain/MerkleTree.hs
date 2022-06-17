{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | This module
module Test.TrustlessSidechain.MerkleTree (test) where

import PlutusPrelude (NonEmpty ((:|)))
import PlutusTx.Builtins.Class qualified as Builtins
import PlutusTx.Prelude
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Property)
import Test.QuickCheck qualified as QuickCheck
import Test.QuickCheck.Modifiers (NonEmptyList (getNonEmpty))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck qualified as QuickCheck
import TrustlessSidechain.MerkleTree (MerkleTree)
import TrustlessSidechain.MerkleTree qualified as MT
import Prelude

-- | 'MerkleListTestCase' new type wrapper to help generate test cases.
newtype MerkleListTestCase = MerkleListTestCase (NonEmpty BuiltinByteString)
  deriving stock (Prelude.Show)

instance Arbitrary MerkleListTestCase where
  arbitrary = do
    lst <- Prelude.fmap getNonEmpty (arbitrary :: Gen (NonEmptyList Prelude.String))
    let a : as = lst
    return $ MerkleListTestCase $ Prelude.fmap Builtins.stringToBuiltinByteString $ a :| as

-- TODO: implement the 'QuickCheck.shrink' function sometime.

{- | Property.
    Let lst be an arbitrary non empty list.
        height (fromNonEmpty lst) <= floor(log_2 (length lst)) + 2
-}
prop_logProofLength :: MerkleListTestCase -> Property
prop_logProofLength (MerkleListTestCase lst@(a :| as)) =
  QuickCheck.property $
    MT.height tree
      <= Prelude.floor
        (Prelude.logBase @Prelude.Double 2 (Prelude.fromIntegral (length lst')))
      + 2
  where
    tree = MT.fromNonEmpty lst
    lst' = a : as

{- | Property.
  x \in lst ==> isJust (MT.lookupMP x (MT.fromNonEmpty lst))
-}
prop_lookupMPHasProof :: MerkleListTestCase -> Property
prop_lookupMPHasProof (MerkleListTestCase lst@(a :| as)) =
  QuickCheck.property $
    all
      (isJust . flip MT.lookupMP tree)
      lst'
  where
    tree = MT.fromNonEmpty lst
    lst' = a : as

{- | Property.
  x \in lst <== isJust (MT.lookupMP x (MT.fromNonEmpty lst))

 but we test this via the contrapositive.
-}
prop_notInLookupMPFail :: Prelude.String -> MerkleListTestCase -> Property
prop_notInLookupMPFail str (MerkleListTestCase lst@(a :| as)) =
  (str' `notElem` lst') QuickCheck.==> isNothing (MT.lookupMP str' tree)
  where
    tree = MT.fromNonEmpty lst
    str' = Builtins.stringToBuiltinByteString str
    lst' = a : as

{- | Property.
 Suppose lst is an arbitrary non empty list.
  Let tree = fromNonEmpty lst

  Just prf = lookupMP x tree ==> memberMP x prf (rootHash tree) = True

 TODO: The converse of this is trickier to test, and the large search space
 makes me doubt the usefulness of QuickCheck for this.
-}
prop_inListMemberMP :: MerkleListTestCase -> Property
prop_inListMemberMP (MerkleListTestCase lst@(a :| as)) =
  QuickCheck.property $
    all
      (\x -> let Just prf = MT.lookupMP x tree in MT.memberMP x prf (MT.rootHash tree))
      lst'
  where
    tree = MT.fromNonEmpty lst
    lst' = a : as

-- This is needed because of QuickCheck. It's explained in the QuickCheck
-- documentation.
return []

test :: TestTree
test =
  Tasty.testGroup
    "MerkleTree"
    [ QuickCheck.testProperties "Properties" $(QuickCheck.allProperties)
    ]
