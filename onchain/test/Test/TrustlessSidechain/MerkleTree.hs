{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Needed for Arbitrary instances for Plutus types
{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module is a bunch of tests for our MerkleTree implementation. For
 now, it's just property based tests.
-}
module Test.TrustlessSidechain.MerkleTree (test) where

import Control.Monad (guard, void)
import Data.Kind (Type)
import Data.List qualified as List
import GHC.Float (Double)
import PlutusPrelude (NonEmpty ((:|)))
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  Gen,
  Property,
  checkCoverage,
  counterexample,
  cover,
  forAllShrinkShow,
  property,
  (===),
 )
import Test.QuickCheck.Gen qualified as Gen
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import TrustlessSidechain.MerkleTree qualified as MT
import Prelude qualified

test :: TestTree
test =
  adjustOption go
    . testGroup
      "MerkleTree"
    $ [ testProperty "height is logarithmic to size" logProofLength
      , testProperty "proof in source iff proof in tree" lookupMpProof
      , testProperty "proof in tree iff proof in root" inListMemberMp
      , testProperty "linear number of lookups" lookupsLinear
      , testProperty "key in source iff proof and hash in lookups" lookupsProof
      ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = Prelude.max 10_000

-- Properties

-- Verify that the height of a Merkle tree is logarithmic in the number of
-- entries
logProofLength :: Property
logProofLength = forAllHelper $ \lst ->
  let tree = MT.fromNonEmpty lst
      heightLimit =
        Prelude.floor (Prelude.logBase @Double 2 (Prelude.fromIntegral (neLength lst))) + 2
      measuredHeight = MT.height tree
   in counterexample (go heightLimit measuredHeight)
        . property
        $ measuredHeight <= heightLimit
  where
    go :: Integer -> Integer -> Prelude.String
    go heightLimit measuredHeight =
      "Measured height: "
        <> Prelude.show measuredHeight
        <> " , height limit: "
        <> Prelude.show heightLimit

-- Verify that presence in list if and only if presence in tree from list
lookupMpProof :: Property
lookupMpProof = forAllNonEmptyPlus $ \ne x ->
  let tree = MT.fromNonEmpty ne
   in (void . MT.lookupMp x $ tree) === (void . neFind x $ ne)

-- Check that:
--

-- * If something is present in the tree, its proof is in the root hash

-- * If something is not present in the tree, no possible proof could be in the

-- root hash
inListMemberMp :: Property
inListMemberMp = forAllShrinkShow arbitrary shrink Prelude.show $ \x ->
  checkCoverage
    . cover 50.0 (isPresent x) "present case"
    . cover 50.0 (not . isPresent $ x) "absent case"
    $ case x of
      Present ne y ->
        let tree = MT.fromNonEmpty ne
         in property $ case MT.lookupMp y tree of
              Nothing -> False -- fail immediately
              Just prf -> MT.memberMp y prf . MT.rootHash $ tree
      Absent ne y proof ->
        let tree = MT.fromNonEmpty ne
         in property $ case MT.lookupMp y tree of
              Nothing -> not . MT.memberMp y proof . MT.rootHash $ tree
              Just _ -> False -- fail immediately

-- Verify that there are exactly n lookups for a source of length n
lookupsLinear :: Property
lookupsLinear = forAllHelper $ \lst ->
  let tree = MT.fromNonEmpty lst
   in (length . MT.lookupsMp $ tree) === neLength lst

-- Verify that, for any item in the source (assuming all elements unique), there
-- is a Merkle proof in the tree for it, with a corresponding root hash;
-- otherwise, there should be nothing.
lookupsProof :: Property
lookupsProof = forAllLookupsWrapper $ \ne x ->
  let tree = MT.fromNonEmpty ne
      lookups = MT.lookupsMp tree
      rootHash = MT.hashLeaf x
   in property $ case MT.lookupMp x tree of
        -- Absent case, make sure we don't have a root hash
        Nothing -> isNothing . List.find (go rootHash) $ lookups
        -- Present case, make sure that we have a root hash and a proof
        Just proof -> isJust . List.find (go2 rootHash proof) $ lookups
  where
    go :: MT.RootHash -> (MT.RootHash, MT.MerkleProof) -> Bool
    go needle (rootHash, _) = needle Prelude.== rootHash
    go2 :: MT.RootHash -> MT.MerkleProof -> (MT.RootHash, MT.MerkleProof) -> Bool
    go2 needleHash needleProof (rootHash, proof) =
      needleHash Prelude.== rootHash Prelude.&& needleProof Prelude.== proof

-- Helpers

-- Orphan Arbitrary for BuiltinByteString
instance Arbitrary BuiltinByteString where
  arbitrary = do
    byteList :: [Integer] <- liftArbitrary $ Gen.choose (0, 255)
    Prelude.pure . Prelude.foldr Builtins.consByteString Builtins.emptyByteString $ byteList
  shrink bbs = do
    let len = Builtins.lengthOfByteString bbs
    guard (len > 0)
    start <- [0 .. len - 1]
    len' <- [1 .. len]
    guard (len' <= len - start)
    Prelude.pure . Builtins.sliceByteString start len' $ bbs

-- Orphan 'Arbitrary1' for Plutus NonEmpty
instance Arbitrary1 NonEmpty where
  liftArbitrary gen = do
    h <- gen
    t <- liftArbitrary gen
    Prelude.pure $ h :| t
  liftShrink shr (h :| t) = do
    h' <- shr h
    t' <- liftShrink shr t
    Prelude.pure $ h' :| t'

-- Wrapper for NonEmpty with only unique elements in it
--
-- Note: do not use the constructor to build these directly! You could easily
-- violate invariants by doing so.
newtype UniqueNonEmpty a = UniqueNonEmpty (NonEmpty a)

instance (Arbitrary a, Prelude.Eq a) => Arbitrary (UniqueNonEmpty a) where
  arbitrary = Prelude.fmap UniqueNonEmpty $ do
    h <- arbitrary
    t <- arbitrary
    Prelude.pure $ case List.uncons . List.nub $ h : t of
      -- Technically this is impossible, but if it happens, we just make an
      -- empty list of just the head.
      Nothing -> h :| []
      Just (h', t') -> h' :| t'

  -- To ensure our invariant doesn't break, we only drop list elements,
  -- never shrink them.
  shrink (UniqueNonEmpty xs) =
    Prelude.fmap UniqueNonEmpty (liftShrink (const []) xs)

-- Wrapper for NonEmpty together with either an element it must include, or an
-- element it must exclude.
--
-- We make it equally probable that either case happens, as well as ensuring we
-- never 'shrink out of case'.
--
-- Note: do not use constructors to build these directly! You could easily
-- violate invariants by doing so.
data NonEmptyPlus a
  = Including (NonEmpty a) a
  | Excluding (NonEmpty a) a
  deriving stock (Prelude.Show)

instance (Arbitrary a, Prelude.Eq a) => Arbitrary (NonEmptyPlus a) where
  arbitrary = Gen.oneof [mkIncluding, mkExcluding]
    where
      -- We generate a non-empty list of a, pick a random element, and go with
      -- that
      mkIncluding :: Gen (NonEmptyPlus a)
      mkIncluding = do
        ne@(x :| _) <- liftArbitrary arbitrary
        Prelude.pure $ Including ne x
      -- Generate a non-empty list with only unique elements in it, pick a
      -- random element, and go with that
      mkExcluding :: Gen (NonEmptyPlus a)
      mkExcluding = do
        UniqueNonEmpty ne <- arbitrary
        case ne of
          (x :| []) -> do
            -- Generate an item different to us
            y <- arbitrary `Gen.suchThat` (Prelude./= x)
            Prelude.pure $ Excluding (y :| []) x
          (x :| (y : ys)) -> Prelude.pure $ Excluding (y :| ys) x
  shrink = \case
    -- To avoid breaking the invariant, we 'crack' the list around the included
    -- element, shrink both sides, then reassemble.
    Including (h :| t) x -> case crackList x (h : t) of
      -- This is technically impossible, so in this case, we simply refuse to
      -- shrink
      Nothing -> []
      Just (pre, post) -> do
        pre' <- shrink pre
        post' <- shrink post
        let combined = pre' List.++ [x] List.++ post'
        -- This is safe, as we know there must be at least one element present.
        let shrunk = List.head combined :| List.tail combined
        Prelude.pure $ Including shrunk x
    -- To avoid breaking the invariant, we only shrink structurally, and don't
    -- touch the omitted element
    Excluding xs x -> do
      xs' <- liftShrink (const []) xs
      Prelude.pure $ Excluding xs' x

-- Checks which case we're in
isInclusive :: NonEmptyPlus a -> Bool
isInclusive = \case
  Including _ _ -> True
  _ -> False

-- Unwrap into components. This 'forgets' inclusion or exclusion.
getContents :: NonEmptyPlus a -> (NonEmpty a, a)
getContents = \case
  Including xs x -> (xs, x)
  Excluding xs x -> (xs, x)

-- Similar to NonEmptyPlus, but in the 'excludes' case, it also picks a possible
-- proof from the tree to show the counter-example.
data MerkleLookupProof
  = Present (NonEmpty BuiltinByteString) BuiltinByteString
  | Absent (NonEmpty BuiltinByteString) BuiltinByteString MT.MerkleProof
  deriving stock (Prelude.Show)

instance Arbitrary MerkleLookupProof where
  arbitrary = do
    nePlus <- arbitrary
    case nePlus of
      Including ne x -> Prelude.pure . Present ne $ x
      Excluding ne x -> do
        let tree = MT.fromNonEmpty ne
        let allProofs = Prelude.fmap snd . MT.lookupsMp $ tree
        proof <- Gen.elements allProofs
        Prelude.pure . Absent ne x $ proof
  shrink = \case
    Present ne x -> do
      (ne', x') <- Prelude.fmap getContents . shrink . Including ne $ x
      Prelude.pure . Present ne' $ x'
    Absent ne x _ -> do
      (ne', x') <- Prelude.fmap getContents . shrink . Excluding ne $ x
      let tree = MT.fromNonEmpty ne'
      let allProofs = Prelude.fmap snd . MT.lookupsMp $ tree
      proof' <- allProofs
      Prelude.pure . Absent ne' x' $ proof'

-- Checks which case we're in
isPresent :: MerkleLookupProof -> Bool
isPresent = \case
  Present _ _ -> True
  _ -> False

-- Similar to NonEmptyPlus, except it always produces lists of unique elements
data LookupsWrapper
  = InLookups (NonEmpty BuiltinByteString) BuiltinByteString
  | NotInLookups (NonEmpty BuiltinByteString) BuiltinByteString
  deriving stock (Prelude.Show)

instance Arbitrary LookupsWrapper where
  arbitrary = do
    UniqueNonEmpty ne <- arbitrary
    Gen.oneof [mkIn ne, mkNotIn ne]
    where
      mkIn :: NonEmpty BuiltinByteString -> Gen LookupsWrapper
      mkIn ne@(x :| _) = Prelude.pure . InLookups ne $ x
      mkNotIn :: NonEmpty BuiltinByteString -> Gen LookupsWrapper
      mkNotIn (x :| xs) = case xs of
        [] -> do
          y <- arbitrary `Gen.suchThat` (x Prelude./=)
          Prelude.pure . NotInLookups (y :| []) $ x
        (y : ys) -> Prelude.pure . NotInLookups (y :| ys) $ x
  shrink = \case
    -- To preserve the invariant, we crack the list around the present element,
    -- shrink both sides, then reassemble.
    InLookups (h :| t) x -> case crackList x (h : t) of
      -- Technically not possible, so in this case, we simply refuse to shrink
      Nothing -> []
      Just (pre, post) -> do
        pre' <- liftShrink (const []) pre
        post' <- liftShrink (const []) post
        let combined = pre' List.++ [x] List.++ post'
        -- This is safe, as we know there must be least one element present.
        let shrunk = List.head combined :| List.tail combined
        Prelude.pure $ InLookups shrunk x
    -- If our element isn't present, shrinking won't magically make it so.
    NotInLookups ne x -> do
      UniqueNonEmpty ne' <- shrink . UniqueNonEmpty $ ne
      Prelude.pure . NotInLookups ne' $ x

-- Checks which case we're in
inLookups :: LookupsWrapper -> Bool
inLookups = \case
  InLookups _ _ -> True
  _ -> False

-- Get the test data, forgetting where we are
getLookupsData :: LookupsWrapper -> (NonEmpty BuiltinByteString, BuiltinByteString)
getLookupsData = \case
  InLookups ne x -> (ne, x)
  NotInLookups ne x -> (ne, x)

-- Helper for properties to reduce line noise
forAllHelper ::
  (Arbitrary a, Prelude.Show a) =>
  (NonEmpty a -> Property) ->
  Property
forAllHelper =
  forAllShrinkShow
    (liftArbitrary arbitrary)
    (liftShrink shrink)
    Prelude.show

-- Helper for using NonEmptyPlus in properties. Wires in coverage automatically.
forAllNonEmptyPlus ::
  (Prelude.Show a, Arbitrary a, Prelude.Eq a) =>
  (NonEmpty a -> a -> Property) ->
  Property
forAllNonEmptyPlus cb =
  forAllShrinkShow arbitrary shrink Prelude.show $ \x ->
    checkCoverage
      . cover 50.0 (isInclusive x) "present case"
      . cover 50.0 (not . isInclusive $ x) "absent case"
      $ let (ne, y) = getContents x
         in cb ne y

forAllLookupsWrapper ::
  (NonEmpty BuiltinByteString -> BuiltinByteString -> Property) ->
  Property
forAllLookupsWrapper cb =
  forAllShrinkShow arbitrary shrink Prelude.show $ \x ->
    checkCoverage
      . cover 50.0 (inLookups x) "present case"
      . cover 50.0 (not . inLookups $ x) "absent case"
      $ let (ne, y) = getLookupsData x
         in cb ne y

neLength :: NonEmpty a -> Integer
neLength (_ :| xs) = 1 + length xs

neFind :: (Eq a) => a -> NonEmpty a -> Maybe a
neFind needle (stack :| stacks) =
  if needle == stack
    then Prelude.pure needle
    else List.find (== needle) stacks

-- If the element is in the list, return everything before it, and the rest
-- without it, otherwise Nothing.
crackList ::
  forall (a :: Type).
  (Prelude.Eq a) =>
  a ->
  [a] ->
  Maybe ([a], [a])
crackList x = go []
  where
    go :: [a] -> [a] -> Maybe ([a], [a])
    go acc = \case
      -- We never found the element, fail
      [] -> Nothing
      (y : ys) ->
        if x Prelude.== y
          then Just (List.reverse acc, ys)
          else go (y : acc) ys
